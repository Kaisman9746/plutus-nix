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
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- ADD THIS ✔✔✔
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS


------------------------------------------------------------------------
-- DATUM + REDEEMER
------------------------------------------------------------------------

data SubsidyDatum = SubsidyDatum
    { sdFarmerPKH         :: PubKeyHash
    , sdGovPKH            :: PubKeyHash
    , sdRequiredAmount    :: Integer
    , sdLandOwnershipHash :: BuiltinByteString
    , sdRainOK            :: Bool
    , sdCropOK            :: Bool
    , sdCommunityOK       :: Bool
    }
PlutusTx.unstableMakeIsData ''SubsidyDatum


data SubsidyAction
    = ClaimSubsidy
    | ReportFraud
    | UpdateData
PlutusTx.unstableMakeIsData ''SubsidyAction


------------------------------------------------------------------------
-- VALIDATOR
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: SubsidyDatum -> SubsidyAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of

      ------------------------------------------------------------------
      -- 1. FARMER CLAIM SUBSIDY
      ------------------------------------------------------------------
      ClaimSubsidy ->
            traceIfFalse "farmer signature missing"
                (txSignedBy info (sdFarmerPKH dat))

        &&  traceIfFalse "rainfall condition failed"
                (sdRainOK dat)

        &&  traceIfFalse "crop planting verification failed"
                (sdCropOK dat)

        &&  traceIfFalse "community attestation missing"
                (sdCommunityOK dat)

        &&  traceIfFalse "farmer not paid correctly"
                farmerPaidCorrectly


      ------------------------------------------------------------------
      -- 2. GOVERNMENT FRAUD REPORT
      ------------------------------------------------------------------
      ReportFraud ->
            traceIfFalse "government signature missing"
                (txSignedBy info (sdGovPKH dat))

        &&  traceIfFalse "government did not receive reclaimed funds"
                govGetsFunds


      ------------------------------------------------------------------
      -- 3. GOVERNMENT UPDATES ELIGIBILITY
      ------------------------------------------------------------------
      UpdateData ->
            traceIfFalse "government signature missing"
                (txSignedBy info (sdGovPKH dat))


  where
    info :: TxInfo
    info = scriptContextTxInfo ctx


    ------------------------------------------------------------------
    -- Check farmer received subsidy
    ------------------------------------------------------------------
    farmerPaidCorrectly :: Bool
    farmerPaidCorrectly =
        let v = valuePaidTo info (sdFarmerPKH dat)
        in valueOf v adaSymbol adaToken >= sdRequiredAmount dat


    ------------------------------------------------------------------
    -- Check gov reclaimed subsidy
    ------------------------------------------------------------------
    govGetsFunds :: Bool
    govGetsFunds =
        let v = valuePaidTo info (sdGovPKH dat)
        in valueOf v adaSymbol adaToken >= sdRequiredAmount dat


------------------------------------------------------------------------
-- UNTYPED WRAPPER
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @SubsidyDatum d
        red = unsafeFromBuiltinData @SubsidyAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()


validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])


------------------------------------------------------------------------
-- HASH + ADDRESS
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin


plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing


toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val

        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised

        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)

        -- IMPORTANT: Fix ambiguity with explicit era annotation
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress

    in T.unpack (C.serialiseAddress shelleyAddr)


------------------------------------------------------------------------
-- WRITE FILE
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path


------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "subsidy.plutus" validator

    putStrLn "\n--- Subsidy Transparency Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show (plutusValidatorHash validator)
    putStrLn $ "Plutus Address:          " <> P.show plutusScriptAddress
    putStrLn $ "Bech32 Address:          " <> toBech32ScriptAddress network validator
    putStrLn "------------------------------------------------"
    putStrLn "Subsidy transparency validator generated successfully."
