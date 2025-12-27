 {-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Prelude as P
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text as T

import qualified PlutusTx
import PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified Plutus.V2.Ledger.Contexts as Contexts
import qualified Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf)
import Codec.Serialise as Serialise

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

--------------------------------------------------------------------------------
-- Datum & Redeemer
--------------------------------------------------------------------------------

data RemitDatum = RemitDatum
    { rdSender   :: PlutusV2.PubKeyHash
    , rdReceiver :: PlutusV2.PubKeyHash
    , rdAmount   :: Integer
    , rdDeadline :: PlutusV2.POSIXTime
    }
PlutusTx.unstableMakeIsData ''RemitDatum

data RemitAction = Claim | Refund
PlutusTx.unstableMakeIsData ''RemitAction

--------------------------------------------------------------------------------
-- On-chain validator
--------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: RemitDatum -> RemitAction -> PlutusV2.ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
        Claim  ->
            traceIfFalse "receiver signature missing"
                (Contexts.txSignedBy info (rdReceiver dat))
            &&
            traceIfFalse "receiver not paid ADA"
                receiverPaid
            &&
            traceIfFalse "deadline passed"
                notExpired

        Refund ->
            traceIfFalse "sender signature missing"
                (Contexts.txSignedBy info (rdSender dat))
            &&
            traceIfFalse "too early to refund"
                afterDeadline
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    range :: PlutusV2.POSIXTimeRange
    range = PlutusV2.txInfoValidRange info

    notExpired :: Bool
    notExpired = Interval.contains (Interval.to (rdDeadline dat)) range

    afterDeadline :: Bool
    afterDeadline =
        Interval.contains (Interval.from (rdDeadline dat + 1)) range

    receiverPaid :: Bool
    receiverPaid =
        let v = Contexts.valuePaidTo info (rdReceiver dat)
        in valueOf v PlutusV2.adaSymbol PlutusV2.adaToken >= rdAmount dat

--------------------------------------------------------------------------------
-- Untyped Wrapper
--------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped
    :: PlutusV2.BuiltinData
    -> PlutusV2.BuiltinData
    -> PlutusV2.BuiltinData
    -> ()
mkValidatorUntyped d r c =
    let dat = PlutusTx.unsafeFromBuiltinData @RemitDatum d
        red = PlutusTx.unsafeFromBuiltinData @RemitAction r
        ctx = PlutusTx.unsafeFromBuiltinData @PlutusV2.ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: PlutusV2.Validator
validator =
    PlutusV2.mkValidatorScript
        $$(PlutusTx.compile [|| mkValidatorUntyped ||])

--------------------------------------------------------------------------------
-- Build Script Address
--------------------------------------------------------------------------------

buildScriptAddress :: PlutusV2.Validator -> C.NetworkId -> P.String
buildScriptAddress v network =
    let script     = PlutusV2.unValidatorScript v
        serialized = Serialise.serialise script
        shortBs    = SBS.toShort (LBS.toStrict serialized)

        plutusScript :: CS.PlutusScript CS.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised shortBs

        scriptObj  = C.PlutusScript C.PlutusScriptV2 plutusScript
        scriptHash = C.hashScript scriptObj
        cred       = C.PaymentCredentialByScript scriptHash
        shelleyAddr = C.makeShelleyAddress network cred C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

--------------------------------------------------------------------------------
-- Write Validator
--------------------------------------------------------------------------------

writeValidator :: P.FilePath -> PlutusV2.Validator -> P.IO ()
writeValidator file v = do
    let bs      = Serialise.serialise (PlutusV2.unValidatorScript v)
        shortBs = SBS.toShort (LBS.toStrict bs)
        lazyBs  = LBS.fromStrict (SBS.fromShort shortBs)
    LBS.writeFile file lazyBs
    P.putStrLn ("Wrote validator to: " P.<> file)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: P.IO ()
main = do
    let outFile = "cross-border.plutus"
    writeValidator outFile validator

    -- CBOR HEX
    let bs      = Serialise.serialise (PlutusV2.unValidatorScript validator)
        strict  = LBS.toStrict bs
        cborHex = B16.encode strict
    LBS.writeFile "cross-border.cborhex" (LBS.fromStrict cborHex)
    P.putStrLn "Wrote CBOR Hex to: cross-border.cborhex"

    -- Script Address (TESTNET)
    let network = C.Testnet (C.NetworkMagic 2)
    let addr = buildScriptAddress validator network
    P.putStrLn "Bech32 Script Address:"
    P.putStrLn addr
                     
