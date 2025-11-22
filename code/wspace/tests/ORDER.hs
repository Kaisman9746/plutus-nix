{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module ORDER where

-- Keep only a tiny Prelude surface for host-side IO & formatting.
import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus Tx and on-chain Prelude (bring the Plutus Eq and operators)
import PlutusTx
import PlutusTx.Prelude
  ( Eq(..)
  , Bool(..)
  , Maybe(..)
  , Integer
  , (==)
  , (/=)
  , (&&)
  , (>)
  , (<)
  , (>=)
  , (<=)
  , (+)
  , (-)
  , (*)
  , divide
  , traceIfFalse
  , traceError
  , map
  )

import qualified PlutusTx.Builtins as Builtins

-- Plutus ledger / contexts
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)

-- Serialization / Cardano API for off-chain helpers
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

--------------------------------------------------------------------------------
-- Asset
--------------------------------------------------------------------------------

data Asset = Asset
    { aCurrency :: CurrencySymbol
    , aToken    :: TokenName
    }
PlutusTx.unstableMakeIsData ''Asset

{-# INLINABLE eqAsset #-}
eqAsset :: Asset -> Asset -> Bool
eqAsset a b =
    aCurrency a == aCurrency b &&
    aToken a    == aToken b

-- Use Plutus Eq (imported above) for on-chain equality
instance Eq Asset where
    {-# INLINABLE (==) #-}
    (==) = eqAsset

--------------------------------------------------------------------------------
-- Side
--------------------------------------------------------------------------------

data Side = Buy | Sell
PlutusTx.unstableMakeIsData ''Side

{-# INLINABLE eqSide #-}
eqSide :: Side -> Side -> Bool
eqSide Buy  Buy  = True
eqSide Sell Sell = True
eqSide _    _    = False

instance Eq Side where
    {-# INLINABLE (==) #-}
    (==) = eqSide

--------------------------------------------------------------------------------
-- OrderDatum
--------------------------------------------------------------------------------

data OrderDatum = OrderDatum
    { odOwner        :: PubKeyHash
    , odBase         :: Asset
    , odQuote        :: Asset
    , odSide         :: Side
    , odLimitNum     :: Integer
    , odLimitDen     :: Integer
    , odQty          :: Integer
    , odRemaining    :: Integer
    , odAllowPartial :: Bool
    , odExpiry       :: POSIXTime
    , odFeeNum       :: Integer
    , odFeeDen       :: Integer
    , odFeeRecipient :: Maybe PubKeyHash
    }
PlutusTx.unstableMakeIsData ''OrderDatum

{-# INLINABLE eqOrderDatum #-}
eqOrderDatum :: OrderDatum -> OrderDatum -> Bool
eqOrderDatum a b =
       odOwner a        == odOwner b
    && odBase a         == odBase b
    && odQuote a        == odQuote b
    && odSide a         == odSide b
    && odLimitNum a     == odLimitNum b
    && odLimitDen a     == odLimitDen b
    && odQty a          == odQty b
    && odRemaining a    == odRemaining b
    && odAllowPartial a == odAllowPartial b
    && odExpiry a       == odExpiry b
    && odFeeNum a       == odFeeNum b
    && odFeeDen a       == odFeeDen b
    && odFeeRecipient a == odFeeRecipient b

instance Eq OrderDatum where
    {-# INLINABLE (==) #-}
    (==) = eqOrderDatum

--------------------------------------------------------------------------------
-- Redeemer
--------------------------------------------------------------------------------

data OrderRedeemer
    = Fill
        { fAmount       :: Integer
        , fExecPriceNum :: Integer
        , fExecPriceDen :: Integer
        }
    | Cancel
PlutusTx.unstableMakeIsData ''OrderRedeemer

--------------------------------------------------------------------------------
-- Helpers (on-chain)
--------------------------------------------------------------------------------

{-# INLINABLE safeMulDiv #-}
safeMulDiv :: Integer -> Integer -> Integer -> Integer
safeMulDiv num mul den =
    if den == 0 then traceError "division by zero" else (num * mul) `divide` den

{-# INLINABLE assetValueOf #-}
assetValueOf :: Value -> Asset -> Integer
assetValueOf v (Asset cs tn) = valueOf v cs tn

{-# INLINABLE txOutHasDatum #-}
txOutHasDatum :: TxOut -> Bool
txOutHasDatum o =
    case txOutDatum o of
        NoOutputDatum     -> False
        OutputDatum _     -> True
        OutputDatumHash _ -> True

{-# INLINABLE getContinuingDatums #-}
getContinuingDatums :: ScriptContext -> [Datum]
getContinuingDatums ctx =
    let outs = getContinuingOutputs ctx
        info = scriptContextTxInfo ctx

        getDatumFromOut :: TxOut -> Datum
        getDatumFromOut o =
            case txOutDatum o of
                OutputDatum d       -> d
                OutputDatumHash dh  ->
                    case findDatum dh info of
                        Nothing -> traceError "continuing output datum not found"
                        Just d  -> d
                NoOutputDatum       -> traceError "expected datum on continuing output"
    in map getDatumFromOut outs

{-# INLINABLE getContinuingOrderDatums #-}
getContinuingOrderDatums :: ScriptContext -> [OrderDatum]
getContinuingOrderDatums ctx =
    let datums = getContinuingDatums ctx
    in map (\(Datum d) -> unsafeFromBuiltinData d) datums

{-# INLINABLE containsBeforeOrAt #-}
containsBeforeOrAt :: POSIXTime -> TxInfo -> Bool
containsBeforeOrAt expiry info =
    Interval.contains (Interval.to expiry) (txInfoValidRange info)

{-# INLINABLE priceMeetsLimit #-}
priceMeetsLimit :: Side -> Integer -> Integer -> Integer -> Integer -> Bool
priceMeetsLimit s limitN limitD execN execD =
    case s of
        Sell -> (execN * limitD) >= (limitN * execD)
        Buy  -> (execN * limitD) <= (limitN * execD)

{-# INLINABLE feeForQuote #-}
feeForQuote :: Integer -> Integer -> Integer -> Integer
feeForQuote feeN feeD quoteAmount =
    if feeD == 0 then traceError "zero fee denominator" else (quoteAmount * feeN) `divide` feeD

{-# INLINABLE txSignedByAny #-}
txSignedByAny :: TxInfo -> PubKeyHash -> Bool
txSignedByAny = txSignedBy

--------------------------------------------------------------------------------
-- Validator
--------------------------------------------------------------------------------

{-# INLINABLE mkOrderValidator #-}
mkOrderValidator :: OrderDatum -> OrderRedeemer -> ScriptContext -> Bool

-- Cancel path
mkOrderValidator od Cancel ctx =
    traceIfFalse "cancel: owner signature required" (txSignedBy info (odOwner od))
  where
    info = scriptContextTxInfo ctx

-- Fill path
mkOrderValidator od (Fill amt eN eD) ctx =
    traceIfFalse "fill: non-positive amount" (amt > 0) &&
    traceIfFalse "fill: not enough remaining" (amt <= odRemaining od) &&
    traceIfFalse "fill: partial fills disabled" partialOK &&
    traceIfFalse "fill: price does not meet limit" priceOK &&
    traceIfFalse "fill: must be before expiry" timeOK &&
    traceIfFalse "fill: maker did not receive expected quote/base" makerReceivedOK &&
    traceIfFalse "fill: continuing datum mismatch" continuingDatumOK
  where
    info = scriptContextTxInfo ctx

    -- partial fill rule
    partialOK =
      if odAllowPartial od then True else (amt == odRemaining od)

    -- price check
    priceOK = priceMeetsLimit (odSide od) (odLimitNum od) (odLimitDen od) eN eD

    -- time-in-force
    timeOK = containsBeforeOrAt (odExpiry od) info

    -- compute expected quote (quote units per base unit)
    expectedQuote = safeMulDiv amt eN eD

    -- fee handling
    feeAmount = feeForQuote (odFeeNum od) (odFeeDen od) expectedQuote

    makerNetQuote = expectedQuote - feeAmount

    makerReceivedOK =
        case odSide od of
            Sell ->
                let got = assetValueOf (valuePaidTo info (odOwner od)) (odQuote od)
                in got >= makerNetQuote
            Buy ->
                let got = assetValueOf (valuePaidTo info (odOwner od)) (odBase od)
                in got >= amt

    -- continuing outputs / remaining
    continuingDatums = getContinuingOrderDatums ctx
    expectedRemaining = odRemaining od - amt

    continuingDatumOK =
        if expectedRemaining > 0
        then case continuingDatums of
            [nd] ->
                   odOwner nd        == odOwner od
                && odBase nd         == odBase od
                && odQuote nd        == odQuote od
                && odSide nd         == odSide od
                && odLimitNum nd     == odLimitNum od
                && odLimitDen nd     == odLimitDen od
                && odQty nd          == odQty od
                && odRemaining nd    == expectedRemaining
                && odAllowPartial nd == odAllowPartial od
                && odExpiry nd       == odExpiry od
            _ -> traceError "expected exactly one continuing output"
        else case continuingDatums of
            [] -> True
            _  -> traceError "expected no continuing outputs"

--------------------------------------------------------------------------------
-- Untyped wrapper
--------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let od  = unsafeFromBuiltinData @OrderDatum d
        red = unsafeFromBuiltinData @OrderRedeemer r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkOrderValidator od red ctx then () else traceError "validation failed"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

--------------------------------------------------------------------------------
-- Hash, Address, File IO
--------------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
    in PlutusV2.ValidatorHash (Builtins.toBuiltin strictBS)

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

-- FIXED VERSION: Explicit era annotation required and correct arg order
toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised   = SBS.toShort (LBS.toStrict (Serialise.serialise val))
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash   = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)

        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn ("Validator written to: " <> path)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "order-validator.plutus" validator

    let vh     = plutusValidatorHash validator
        onAddr = plutusScriptAddress
        bech32 = toBech32ScriptAddress network validator

    putStrLn "\n--- On-chain Limit Order Validator Info ---"
    putStrLn ("Validator Hash (Plutus): " <> P.show vh)
    putStrLn ("Plutus Script Address:    " <> P.show onAddr)
    putStrLn ("Bech32 Script Address:    " <> bech32)
    putStrLn "-------------------------------------------"
    putStrLn "Order validator generated successfully."
