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

import qualified PlutusTx
import PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified Plutus.V2.Ledger.Contexts as Contexts
import qualified Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf)
import Codec.Serialise as Serialise

--------------------------------------------------------------------------------
-- Datum & Redeemer
--------------------------------------------------------------------------------

data RemitDatum = RemitDatum
    { rdSender   :: PlutusV2.PubKeyHash
    , rdReceiver :: PlutusV2.PubKeyHash
    , rdAmount   :: Integer          -- amount in lovelace
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
        Claim  -> traceIfFalse "receiver signature missing" (Contexts.txSignedBy info (rdReceiver dat))
                  && traceIfFalse "receiver not paid ADA" receiverPaid
                  && traceIfFalse "deadline passed" notExpired

        Refund -> traceIfFalse "sender signature missing" (Contexts.txSignedBy info (rdSender dat))
                  && traceIfFalse "too early to refund" afterDeadline
  where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    range :: PlutusV2.POSIXTimeRange
    range = PlutusV2.txInfoValidRange info

    notExpired :: Bool
    notExpired = Interval.contains (Interval.to (rdDeadline dat)) range

    afterDeadline :: Bool
    afterDeadline = Interval.contains (Interval.from (rdDeadline dat + 1)) range

    receiverPaid :: Bool
    receiverPaid =
        let v = Contexts.valuePaidTo info (rdReceiver dat)
        in valueOf v PlutusV2.adaSymbol PlutusV2.adaToken >= rdAmount dat

--------------------------------------------------------------------------------
-- Untyped wrapper
--------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: PlutusV2.BuiltinData -> PlutusV2.BuiltinData -> PlutusV2.BuiltinData -> ()
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
-- Validator hash
--------------------------------------------------------------------------------

validatorHash' :: PlutusV2.Validator -> PlutusV2.ValidatorHash
validatorHash' v =
    let bytes   = Serialise.serialise v
        strict  = LBS.toStrict bytes
        builtin = PlutusV2.toBuiltin strict
    in PlutusV2.ValidatorHash builtin

--------------------------------------------------------------------------------
-- Write validator to raw file
--------------------------------------------------------------------------------

writeValidator :: P.FilePath -> PlutusV2.Validator -> P.IO ()
writeValidator file v = do
    let bs      = Serialise.serialise (PlutusV2.unValidatorScript v)
        shortBs = SBS.toShort (LBS.toStrict bs)
        lazyBs  = LBS.fromStrict $ SBS.fromShort shortBs
    LBS.writeFile file lazyBs
    P.putStrLn ("Wrote validator to: " P.<> file)

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: P.IO ()
main = do
    let outFile = "cross-border.plutus"
    writeValidator outFile validator
