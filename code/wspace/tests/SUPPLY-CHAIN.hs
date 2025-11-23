{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API (Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-----------------------------------------------------------------------------------------
-- SUPPLY-CHAIN DATUM / REDEEMER
-----------------------------------------------------------------------------------------

data Step = Step
    { stepName   :: BuiltinByteString
    , stepAmount :: Integer
    }
PlutusTx.unstableMakeIsData ''Step

data MilestoneEscrow = MilestoneEscrow
    { meBuyer     :: PubKeyHash
    , meSupplier  :: PubKeyHash
    , meSteps     :: [Step]
    , meCurrent   :: Integer
    }
PlutusTx.unstableMakeIsData ''MilestoneEscrow

data SupplyAction = Advance | Recall
PlutusTx.unstableMakeIsData ''SupplyAction

-----------------------------------------------------------------------------------------
-- VALIDATOR LOGIC
-----------------------------------------------------------------------------------------

{-# INLINABLE validateStep #-}
validateStep :: MilestoneEscrow -> ScriptContext -> Bool
validateStep dat ctx =
    let info = scriptContextTxInfo ctx
        curr = meCurrent dat
        steps = meSteps dat
        paidToSupplier =
            if curr < 0 || curr >= length steps
            then traceError "invalid step index"
            else
                let st = nth steps curr
                    v = valuePaidTo info (meSupplier dat)
                in traceIfFalse "supplier underpaid"
                      (valueOf v adaSymbol adaToken >= stepAmount st)
    in paidToSupplier

{-# INLINABLE nth #-}
nth :: [a] -> Integer -> a
nth xs i =
    case xs of
        []      -> traceError "index OOB"
        (y:ys)  -> if i == 0 then y else nth ys (i - 1)

{-# INLINABLE mkValidator #-}
mkValidator :: MilestoneEscrow -> SupplyAction -> ScriptContext -> Bool
mkValidator dat act ctx =
    case act of
        Advance ->
            traceIfFalse "buyer sig missing"  (txSignedBy info (meBuyer dat))    &&
            validateStep dat ctx

        Recall  ->
            traceIfFalse "supplier sig missing" (txSignedBy info (meSupplier dat))
  where
    info = scriptContextTxInfo ctx

-----------------------------------------------------------------------------------------
-- UNTYPED VALIDATOR WRAPPER
-----------------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @MilestoneEscrow d
        red = unsafeFromBuiltinData @SupplyAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-----------------------------------------------------------------------------------------
-- ON-CHAIN HASH + ADDRESS
-----------------------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bs     = Serialise.serialise val
        short  = SBS.toShort (LBS.toStrict bs)
        strict = SBS.fromShort short
    in PlutusV2.ValidatorHash (Builtins.toBuiltin strict)

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

-----------------------------------------------------------------------------------------
-- 🚀 FINAL CORRECT VERSION — FIXED BECH32 ADDRESS
-----------------------------------------------------------------------------------------

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val

        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised

        scriptHash :: C.ScriptHash
        scriptHash =
            C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)

        -- Correct 3-argument call, era is in the type annotation ONLY
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress

    in T.unpack (C.serialiseAddress shelleyAddr)

-----------------------------------------------------------------------------------------
-- WRITE SCRIPT FILE
-----------------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn ("Wrote script to: " <> path)

-----------------------------------------------------------------------------------------
-- MAIN
-----------------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "supply-chain.plutus" validator

    let vh     = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- SUPPLY-CHAIN VALIDATOR ---"
    putStrLn ("Validator Hash: " <> P.show vh)
    putStrLn ("Plutus Address: " <> P.show onchain)
    putStrLn ("Bech32 Address: " <> bech32)
    putStrLn "--------------------------------"
    putStrLn "Supply-chain validator generated successfully."
