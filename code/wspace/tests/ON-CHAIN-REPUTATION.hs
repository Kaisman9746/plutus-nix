{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))

-- Plutus
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Base16 as B16

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum & Redeemer
------------------------------------------------------------------------

data EscrowDatum = EscrowDatum
    { edEmployer            :: PubKeyHash
    , edFreelancer          :: PubKeyHash
    , edAmountPerMilestone  :: Integer
    , edMilestonesRemaining :: Integer
    , edReputation          :: Integer
    , edDeadline            :: POSIXTime
    }
    deriving (P.Show, Generic)

PlutusTx.unstableMakeIsData ''EscrowDatum
PlutusTx.makeLift ''EscrowDatum

data EscrowAction = ApproveMilestone | RefundEmployer
PlutusTx.unstableMakeIsData ''EscrowAction
PlutusTx.makeLift ''EscrowAction

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: EscrowDatum -> EscrowAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      ApproveMilestone ->
           traceIfFalse "script input missing" scriptInputExists &&
           traceIfFalse "employer signature missing" (txSignedBy info (edEmployer dat)) &&
           traceIfFalse "freelancer not paid enough" freelancerPaid &&
           traceIfFalse "datum not updated correctly" correctContinuingDatum

      RefundEmployer ->
           traceIfFalse "script input missing" scriptInputExists &&
           traceIfFalse "employer signature missing" (txSignedBy info (edEmployer dat)) &&
           traceIfFalse "too early for refund" afterDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    scriptInputExists :: Bool
    scriptInputExists =
        case findOwnInput ctx of
          Nothing -> False
          Just _  -> True

    afterDeadline :: Bool
    afterDeadline =
        Interval.contains (Interval.from (edDeadline dat + 1)) (txInfoValidRange info)

    freelancerPaid :: Bool
    freelancerPaid =
        let v = valuePaidTo info (edFreelancer dat)
        in valueOf v adaSymbol adaToken >= edAmountPerMilestone dat

    correctContinuingDatum :: Bool
    correctContinuingDatum =
        case findOwnInput ctx of
          Nothing -> traceError "missing script input"
          Just txIn ->
            let _inpOut = txInInfoResolved txIn
                continuing = getContinuingOutputs ctx
            in if edMilestonesRemaining dat <= 1
                then True
                else case continuing of
                      [] -> traceError "expected continuing output with updated datum"
                      outs -> any (hasUpdatedDatum info dat) outs

    hasUpdatedDatum :: TxInfo -> EscrowDatum -> TxOut -> Bool
    hasUpdatedDatum txinfo old out =
      case txOutDatum out of
        OutputDatumHash dh ->
          case findDatum dh txinfo of
            Nothing -> False
            Just (Datum d) ->
              case PlutusTx.fromBuiltinData d :: Maybe EscrowDatum of
                Nothing -> False
                Just new -> datumMatchesUpdate new old
        OutputDatum (Datum d) ->
          case PlutusTx.fromBuiltinData d :: Maybe EscrowDatum of
            Nothing -> False
            Just new -> datumMatchesUpdate new old
        NoOutputDatum -> False

    datumMatchesUpdate :: EscrowDatum -> EscrowDatum -> Bool
    datumMatchesUpdate new old =
         edEmployer new            == edEmployer old
      && edFreelancer new          == edFreelancer old
      && edAmountPerMilestone new  == edAmountPerMilestone old
      && edMilestonesRemaining new == edMilestonesRemaining old - 1
      && edReputation new          == edReputation old + 1
      && edDeadline new            == edDeadline old

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @EscrowDatum d
        _red = unsafeFromBuiltinData @EscrowAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat _red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Hash & Address
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash v =
    let bytes   = Serialise.serialise v
        short   = SBS.toShort (LBS.toStrict bytes)
        strict  = SBS.fromShort short
        builtin = Builtins.toBuiltin strict
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised   = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript = CS.PlutusScriptSerialised serialised
        cardanoPlutus = C.PlutusScript C.PlutusScriptV2 plutusScript
        scriptHash   = C.hashScript cardanoPlutus

        addr :: C.AddressInEra C.BabbageEra
        addr = C.makeShelleyAddressInEra
                 network
                 (C.PaymentCredentialByScript scriptHash)
                 C.NoStakeAddress
    in T.unpack (C.serialiseAddress addr)

------------------------------------------------------------------------
-- Write validator to file
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

writeCBORHex :: FilePath -> Validator -> IO ()
writeCBORHex path val = do
    let cborBytes = Serialise.serialise val
        cborHex   = B16.encode (LBS.toStrict cborBytes)
    LBS.writeFile path (LBS.fromStrict cborHex)
    putStrLn $ "CBOR Hex written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    -- Write Plutus script
    writeValidator "reputation-escrow.plutus" validator

    -- Write CBOR hex
    writeCBORHex "reputation-escrow.hex" validator

    -- Info outputs
    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Reputation Escrow Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:   " <> P.show onchain
    putStrLn $ "Bech32 Script Address:   " <> bech32
    putStrLn "----------------------------------------"
    putStrLn "Reputation Escrow validator and CBOR hex generated successfully."
