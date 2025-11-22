{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T
import GHC.Generics (Generic)

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

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-------------------------------------------------------------------------
-- Lockbox Datum / Redeemer
-------------------------------------------------------------------------
-- Datum describing a custodial lockbox entry
data LockboxDatum = LockboxDatum
    { lbUser           :: PubKeyHash        -- user who locked the asset
    , lbAssetCurrency  :: CurrencySymbol     -- locked asset currency symbol
    , lbAssetToken     :: TokenName          -- locked asset token name
    , lbAmount         :: Integer            -- amount locked
    , lbCustodian      :: PubKeyHash         -- custodian PKH (must sign for unlock)
    , lbOracle         :: PubKeyHash         -- oracle PKH (must sign to attest burn)
    , lbLockboxNFTCS   :: CurrencySymbol     -- lockbox NFT currency symbol (to identify UTxO)
    , lbLockboxNFTTN   :: TokenName          -- lockbox NFT token name
    , lbFee            :: Integer            -- fee (in lovelace) to pay custodian on unlock
    , lbExpectedBurn   :: BuiltinByteString  -- expected burn proof hash (sha256-like) provided off-chain
    }
PlutusTx.unstableMakeIsData ''LockboxDatum

-- Redeemer: Lock (initial lock) or Unlock with the burn proof provided (as hash bytes)
data LockboxAction = LLock | LUnlock BuiltinByteString
PlutusTx.unstableMakeIsData ''LockboxAction

-------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------

{-# INLINABLE scriptInputContainsNFT #-}
scriptInputContainsNFT :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
scriptInputContainsNFT ctx cs tn =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in valueOf v cs tn >= 1

{-# INLINABLE valueLockedInInput #-}
-- how much of (cs,tn) is present at the script input (the locked UTxO)
valueLockedInInput :: ScriptContext -> CurrencySymbol -> TokenName -> Integer
valueLockedInInput ctx cs tn =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in valueOf v cs tn

{-# INLINABLE paidTo #-}
paidTo :: TxInfo -> PubKeyHash -> Value
paidTo info pkh = valuePaidTo info pkh

{-# INLINABLE adaPaidTo #-}
adaPaidTo :: TxInfo -> PubKeyHash -> Integer
adaPaidTo info pkh = valueOf (valuePaidTo info pkh) adaSymbol adaToken

-------------------------------------------------------------------------
-- Validator Logic
-------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: LockboxDatum -> LockboxAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      LLock ->
           -- Lock action: user locks specified asset and attaches lockbox NFT to the script UTxO.
           traceIfFalse "lock: script input missing lockbox NFT" (scriptInputContainsNFT ctx (lbLockboxNFTCS dat) (lbLockboxNFTTN dat)) &&
           traceIfFalse "lock: locked amount mismatch" (valueLockedInInput ctx (lbAssetCurrency dat) (lbAssetToken dat) >= lbAmount dat) &&
           traceIfFalse "lock: must be signed by user" (txSignedBy info (lbUser dat))
      LUnlock providedBurnHash ->
           -- Unlock action: requires custodian + oracle signatures and the provided burn hash must match expected
           traceIfFalse "unlock: script input missing lockbox NFT" (scriptInputContainsNFT ctx (lbLockboxNFTCS dat) (lbLockboxNFTTN dat)) &&
           traceIfFalse "unlock: custodian signature missing" (txSignedBy info (lbCustodian dat)) &&
           traceIfFalse "unlock: oracle signature missing" (txSignedBy info (lbOracle dat)) &&
           traceIfFalse "unlock: burn proof mismatch" (providedBurnHash == lbExpectedBurn dat) &&
           traceIfFalse "unlock: asset returned to user" (valueOf (valuePaidTo info (lbUser dat)) (lbAssetCurrency dat) (lbAssetToken dat) >= lbAmount dat) &&
           traceIfFalse "unlock: fee not paid to custodian" (adaPaidTo info (lbCustodian dat) >= lbFee dat)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

-------------------------------------------------------------------------
-- Untyped wrapper / Boilerplate
-------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @LockboxDatum d
        red = unsafeFromBuiltinData @LockboxAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------------------------------
-- Validator Hash + Addresses (same pattern as the user's original file)
-------------------------------------------------------------------------

-- Compute validator hash using only plutus-ledger-api + plutus-tx
plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short              -- convert ShortByteString → ByteString
        builtin  = Builtins.toBuiltin strictBS      -- convert ByteString → BuiltinByteString
    in PlutusV2.ValidatorHash builtin

-- Derive the Plutus script address from the hash
plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

-- Off-chain (Cardano API) Bech32 address for CLI use
toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised

        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)

        -- The type annotation declares it's a Babbage-era address
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

-------------------------------------------------------------------------
-- File writing
-------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

-------------------------------------------------------------------------
-- Main (CLI helper and quick info)
-------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "lockbox-validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Lockbox (Bridge Custodial) Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "-------------------------------------------------"
    putStrLn "Lockbox validator generated successfully."
