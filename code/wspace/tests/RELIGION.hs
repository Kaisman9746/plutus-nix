{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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

--------------------------------------------------------------------------------
-- DATA TYPES
--------------------------------------------------------------------------------

data Split = Split
    { spRecipient :: PubKeyHash
    , spShare     :: Integer
    } deriving (Generic)
PlutusTx.unstableMakeIsData ''Split
PlutusTx.makeLift ''Split

data Treasury = Treasury
    { trCommittee   :: [PubKeyHash]
    , trQuorumPerc  :: Integer
    , trMinProposal :: Integer
    } deriving (Generic)
PlutusTx.unstableMakeIsData ''Treasury
PlutusTx.makeLift ''Treasury

data Proposal = Proposal
    { pId         :: BuiltinByteString
    , pProposer   :: PubKeyHash
    , pRecipient  :: PubKeyHash
    , pAmount     :: Integer
    , pPurpose    :: BuiltinByteString
    , pDeadline   :: POSIXTime
    , pYes        :: Integer
    , pNo         :: Integer
    , pExecuted   :: Bool
    , pEarmarkCS  :: Maybe CurrencySymbol
    , pEarmarkTN  :: Maybe TokenName
    } deriving (Generic)
PlutusTx.unstableMakeIsData ''Proposal
PlutusTx.makeLift ''Proposal

data TreasuryDatum = TreasuryDatum
    { tdTreasury  :: Treasury
    , tdProposals :: [Proposal]
    } deriving (Generic)
PlutusTx.unstableMakeIsData ''TreasuryDatum
PlutusTx.makeLift ''TreasuryDatum

data TreasuryAction
    = Donate
    | CreateProposal Proposal
    | Vote Bool BuiltinByteString
    | ExecuteProposal BuiltinByteString
PlutusTx.unstableMakeIsData ''TreasuryAction
PlutusTx.makeLift ''TreasuryAction

--------------------------------------------------------------------------------
-- HELPERS
--------------------------------------------------------------------------------

{-# INLINABLE findProposal #-}
findProposal :: BuiltinByteString -> [Proposal] -> Maybe Proposal
findProposal _ [] = Nothing
findProposal pid (p:ps) =
    if pId p == pid then Just p else findProposal pid ps

{-# INLINABLE totalSupplyOfEarmark #-}
totalSupplyOfEarmark ::
    TxInfo ->
    Maybe CurrencySymbol ->
    Maybe TokenName ->
    PubKeyHash ->
    Integer ->
    Bool
totalSupplyOfEarmark _ Nothing _ _ _ = True
totalSupplyOfEarmark info (Just cs) (Just tn) recipient _ =
    let v = valuePaidTo info recipient
    in valueOf v cs tn >= 1

--------------------------------------------------------------------------------
-- VALIDATOR
--------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: TreasuryDatum -> TreasuryAction -> ScriptContext -> Bool
mkValidator td action ctx =
  case action of

    ----------------------------------------------------------------------------
    -- DONATE
    ----------------------------------------------------------------------------
    Donate ->
        traceIfFalse "donation must increase contract balance" donationIncreases

    ----------------------------------------------------------------------------
    -- CREATE PROPOSAL
    ----------------------------------------------------------------------------
    CreateProposal p ->
        let proposerIsCommittee =
                txSignedBy info (pProposer p)

            noDuplicate =
                case findProposal (pId p) (tdProposals td) of
                    Nothing -> True
                    Just _  -> False

        in  traceIfFalse "proposer not committee member" proposerIsCommittee &&
            traceIfFalse "proposal amount below minimum"
                (pAmount p >= trMinProposal (tdTreasury td)) &&
            traceIfFalse "duplicate proposal id" noDuplicate

    ----------------------------------------------------------------------------
    -- VOTE
    ----------------------------------------------------------------------------
    Vote support pid ->
        case findProposal pid (tdProposals td) of
          Nothing -> traceError "proposal not found"
          Just pr ->
            traceIfFalse "voting period over" (contains (pDeadline pr) txRange) &&
            traceIfFalse "already executed" (not (pExecuted pr)) &&
            traceIfFalse "vote accepted" True

    ----------------------------------------------------------------------------
    -- EXECUTE PROPOSAL
    ----------------------------------------------------------------------------
    ExecuteProposal pid ->
        case findProposal pid (tdProposals td) of
          Nothing -> traceError "proposal not found"
          Just pr ->
            traceIfFalse "voting not finished" (after (pDeadline pr) txRange) &&
            traceIfFalse "already executed" (not (pExecuted pr)) &&
            traceIfFalse "quorum or majority not met"
                (quorumAndMajority pr (tdTreasury td)) &&
            traceIfFalse "earmark not honored"
                (totalSupplyOfEarmark info (pEarmarkCS pr) (pEarmarkTN pr)
                 (pRecipient pr) (pAmount pr)) &&
            traceIfFalse "recipient not paid"
                (valueOf (valuePaidTo info (pRecipient pr)) adaSymbol adaToken
                 >= pAmount pr)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    donationIncreases = True

    contains :: POSIXTime -> POSIXTimeRange -> Bool
    contains t r = Interval.contains (Interval.to t) r

    after :: POSIXTime -> POSIXTimeRange -> Bool
    after d r = Interval.contains (Interval.from (d + 1)) r

    quorumAndMajority :: Proposal -> Treasury -> Bool
    quorumAndMajority pr tr =
        let totalVotes = pYes pr + pNo pr
            quorumNeeded = (trQuorumPerc tr * totalVotes) `divide` 100
            yesMajority = pYes pr > pNo pr
        in totalVotes >= quorumNeeded && yesMajority

--------------------------------------------------------------------------------
-- UNTYPED WRAPPER
--------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @TreasuryDatum d
        red = unsafeFromBuiltinData @TreasuryAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

--------------------------------------------------------------------------------
-- HASH + ADDRESS
--------------------------------------------------------------------------------

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

        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

--------------------------------------------------------------------------------
-- Write file
--------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeValidator "religion_treasury.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Religion Treasury Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Religion treasury validator generated successfully."
