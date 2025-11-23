{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns      #-}

module Main where

import Prelude (IO, String, FilePath, print, putStrLn, show, (<>))
import qualified Prelude as P

import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import Plutus.V2.Ledger.Contexts
import Plutus.V1.Ledger.Interval (after, contains, from, to)

import Codec.Serialise (serialise)
import qualified Data.ByteString.Char8 as C


------------------------------------------------------------------------------------------
-- DATATYPES
------------------------------------------------------------------------------------------

data FarmDatum = FarmDatum
    { fdFarmer           :: PubKeyHash
    , fdCoop             :: PubKeyHash
    , fdInvestment       :: Integer
    , fdProfitShare      :: Integer
    , fdStart            :: POSIXTime
    , fdExpiry           :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''FarmDatum

data FarmRedeemer = Harvest | Refund
PlutusTx.unstableMakeIsData ''FarmRedeemer


------------------------------------------------------------------------------------------
-- VALIDATOR LOGIC
------------------------------------------------------------------------------------------

{-# INLINABLE farmValidator #-}
farmValidator :: FarmDatum -> FarmRedeemer -> ScriptContext -> Bool
farmValidator datum redeemer ctx =
    let
        info   = scriptContextTxInfo ctx
        farmer = fdFarmer datum
        start  = fdStart datum
        expiry = fdExpiry datum
        range  = txInfoValidRange info
    in
    case redeemer of

        Harvest ->
            traceIfFalse "harvest too early" (contains (from start) range) &&
            traceIfFalse "harvest too late"  (contains (to expiry) range)

        Refund ->
            traceIfFalse "refund too early" (contains (from (expiry + 1)) range) &&
            traceIfFalse "farmer not signed" (txSignedBy info farmer)


------------------------------------------------------------------------------------------
-- UNTYPED WRAPPER
------------------------------------------------------------------------------------------

{-# INLINABLE mkWrapped #-}
mkWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrapped d r c =
    let
        datum     = PlutusTx.unsafeFromBuiltinData d
        redeemer  = PlutusTx.unsafeFromBuiltinData r
        context   = PlutusTx.unsafeFromBuiltinData c
    in
    if farmValidator datum redeemer context
        then ()
        else error ()


validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkWrapped ||])


------------------------------------------------------------------------------------------
-- MINTING POLICY
------------------------------------------------------------------------------------------

{-# INLINABLE mkCoopMintPolicy #-}
mkCoopMintPolicy :: PubKeyHash -> ScriptContext -> Bool
mkCoopMintPolicy coop ctx =
    traceIfFalse "coop signature missing" (txSignedBy (scriptContextTxInfo ctx) coop)


-- IMPORTANT: mkUntypedMintingPolicy is in Plutus.V2.Ledger.Contexts
{-# INLINABLE mkUntyped #-}
mkUntyped :: (ScriptContext -> Bool) -> BuiltinData -> BuiltinData -> ()
mkUntyped f _ ctx =
    if f (PlutusTx.unsafeFromBuiltinData ctx)
        then ()
        else error ()


policy :: PubKeyHash -> MintingPolicy
policy pkh =
    mkMintingPolicyScript $
        $$(PlutusTx.compile [||
            \pkh' -> mkUntyped (mkCoopMintPolicy pkh')
        ||])
        `PlutusTx.applyCode`
        PlutusTx.liftCode pkh


------------------------------------------------------------------------------------------
-- SERIALIZATION
------------------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator fp v = do
    let bs = serialise v
    C.writeFile fp (C.pack (show bs))

writePolicy :: FilePath -> MintingPolicy -> IO ()
writePolicy fp p = do
    let bs = serialise p
    C.writeFile fp (C.pack (show bs))


------------------------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "Farming contract compiled successfully."
