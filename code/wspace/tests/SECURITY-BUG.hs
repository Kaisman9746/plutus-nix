{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module SECURITY_BUG where

import Prelude (Show)
import qualified Prelude as P
import GHC.Generics (Generic)

import PlutusTx
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts

------------------------------------------------------------
-- DATA TYPES
------------------------------------------------------------

data MultiDatum
    = MD_Integer Integer
    | MD_Bytes BuiltinByteString
    | MD_Text BuiltinByteString
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''MultiDatum

------------------------------------------------------------
-- VALIDATOR
------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: MultiDatum -> ScriptContext -> Bool
mkValidator _ ctx =
    traceIfFalse "no signer" signerOK
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signerOK :: Bool
    signerOK = length (txInfoSignatories info) > 0

------------------------------------------------------------
-- WRAPPED VALIDATOR
------------------------------------------------------------

{-# INLINABLE mkWrapped #-}
mkWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrapped d c _ =
    let datum = unsafeFromBuiltinData @MultiDatum d
        ctx   = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator datum ctx
       then ()
       else error ()

------------------------------------------------------------
-- COMPILE VALIDATOR
------------------------------------------------------------

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrapped ||])

------------------------------------------------------------
-- CORRECT VALIDATOR HASH (NO MANUAL HASHING)
------------------------------------------------------------

validatorHash' :: ValidatorHash
validatorHash' = validatorHash validator

------------------------------------------------------------
-- SCRIPT ADDRESS
------------------------------------------------------------

scriptAddress :: Address
scriptAddress =
    Address (ScriptCredential validatorHash') Nothing
 