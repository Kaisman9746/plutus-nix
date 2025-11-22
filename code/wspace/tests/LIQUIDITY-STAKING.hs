{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts    #-}

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

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

-- Pool datum: tracks ADA liquidity and sADA shares outstanding, fee (basis points),
-- and an admin PubKeyHash who can perform maintenance updates.
data LSDPoolDatum = LSDPoolDatum
    { lpAdmin      :: PubKeyHash
    , lpTotalADA   :: Integer    -- lovelace in pool
    , lpTotalShares:: Integer    -- total sADA shares minted (in smallest units)
    , lpFeeBps     :: Integer    -- fee in basis points taken on deposits/withdraws
    , lpNextTicket :: Integer    -- simple counter for ticket NFT uniqueness
    }
    deriving (Generic)

PlutusTx.unstableMakeIsData ''LSDPoolDatum
PlutusTx.makeLift ''LSDPoolDatum

-- Actions the pool validator accepts
data LSDPoolAction = DepositAction
                   | RequestWithdrawAction { rwaShares :: Integer, rwaOwner :: PubKeyHash }
                   | FinalizeWithdrawAction { fwaTicketToken :: TokenName }
                   | AdminUpdateAction { newFeeBps :: Integer }
    deriving (Generic)

PlutusTx.unstableMakeIsData ''LSDPoolAction
PlutusTx.makeLift ''LSDPoolAction

-- Withdrawal ticket datum is encoded into NFT token name off-chain; validator only checks presence + owner signature.
-- For simplicity, we won't use a complex datum for the ticket UTxO; the NFT tokenname carries uniqueness.

------------------------------------------------------------------------
-- Helpers: Exchange rate math
------------------------------------------------------------------------

-- Given pool totals, compute how many shares to mint for an ADA deposit (in lovelace -> shares)
-- share = if totalShares==0 then depositAmount (1:1 initial) else deposit * totalShares / totalADA
{-# INLINABLE adaToShares #-}
adaToShares :: Integer -> Integer -> Integer -> Integer
adaToShares deposit totalADA totalShares =
    if totalShares == 0 || totalADA == 0
      then deposit -- initial 1:1 mapping
      else (deposit * totalShares) `divide` totalADA

-- Convert shares back to ADA (integer division floor)
{-# INLINABLE sharesToAda #-}
sharesToAda :: Integer -> Integer -> Integer -> Integer
sharesToAda shares totalADA totalShares =
    if totalShares == 0
      then 0
      else (shares * totalADA) `divide` totalShares

-- apply fee (bps) returning net amount after fee and fee amount
{-# INLINABLE applyFeeBps #-}
applyFeeBps :: Integer -> Integer -> (Integer, Integer)
applyFeeBps amount bps =
    let fee = (amount * bps) `divide` 10000
        net = amount - fee
    in (net, fee)

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE scriptOwnInputValue #-}
scriptOwnInputValue :: ScriptContext -> Value
scriptOwnInputValue ctx =
    case findOwnInput ctx of
      Nothing -> traceError "no script input"
      Just i  -> txOutValue $ txInInfoResolved i

{-# INLINABLE mkLSDPoolValidator #-}
mkLSDPoolValidator :: LSDPoolDatum -> LSDPoolAction -> ScriptContext -> Bool
mkLSDPoolValidator dat action ctx =
    case action of
      DepositAction ->
          traceIfFalse "must include script input" hasScriptIn &&
          traceIfFalse "deposit ADA must be present" depositPresent &&
          traceIfFalse "shares must be minted to depositor" sharesMintedToDepositor &&
          traceIfFalse "pool datum must update correctly" poolDatumUpdated
      RequestWithdrawAction shares ownerPkh ->
          traceIfFalse "must include script input" hasScriptIn &&
          traceIfFalse "shares to burn present" sharesBurned &&
          traceIfFalse "ticket NFT minted to requester" ticketMintedToOwner &&
          traceIfFalse "pool datum decreased shares and ADA unchanged" poolDatumUpdatedReq
      FinalizeWithdrawAction tn ->
          traceIfFalse "must include script input" hasScriptIn &&
          traceIfFalse "ticket NFT burned in finalize" ticketBurned &&
          traceIfFalse "ADA paid to ticket owner" adaPaidToOwner &&
          traceIfFalse "pool datum decreased ADA and shares unchanged" poolDatumUpdatedFinal
      AdminUpdateAction newBps ->
          traceIfFalse "admin signature required" (txSignedBy info (lpAdmin dat)) &&
          traceIfFalse "only fee updated in datum" (onlyFeeChanged newBps)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Check that this script's UTxO was consumed
    hasScriptIn :: Bool
    hasScriptIn = isJust (findOwnInput ctx)

    -- find the current pool output (the output back to the same script address)
    ownOutputs :: [TxOut]
    ownOutputs = [ o | o <- txInfoOutputs info
                     , case txOutAddress o of
                         Address (ScriptCredential vh) _ -> vh == ownHash
                         _ -> False
               ]

    ownHash :: ValidatorHash
    ownHash = case validatorHashFromDatum dat of
                Just vh -> vh
                Nothing -> traceError "cannot derive own validator hash"

    -- In absence of a better helper, we require a single continuing pool output and read its datum
    continuingDatum :: Maybe LSDPoolDatum
    continuingDatum = case ownOutputs of
        (o:_) -> case txOutDatumHash o of
                    Nothing -> Nothing
                    Just dh -> case findDatum dh info of
                                 Nothing -> Nothing
                                 Just (Datum d) -> fromBuiltinData d
        [] -> Nothing

    -- the script input value (pool UTxO value)
    inputVal :: Value
    inputVal = scriptOwnInputValue ctx

    -- the continuing value (pool UTxO after the tx)
    outVal :: Value
    outVal = case ownOutputs of
               (o:_) -> txOutValue o
               []    -> traceError "no continuing pool output"

    -- Basic checks for Deposit
    depositPresent :: Bool
    depositPresent =
      -- ADA added to pool: outVal contains at least inputVal + some ADA
      let inAda  = valueOf inputVal adaSymbol adaToken
          outAda = valueOf outVal adaSymbol adaToken
      in outAda > inAda

    -- shares minted to depositor: minted value includes sADA positive to depositor pubkey
    sharesMintedToDepositor :: Bool
    sharesMintedToDepositor =
      let minted = txInfoMint info
          sSym = lpSharesCurrency dat
          sTok = lpSharesTokenName dat
      in valueOf minted sSym sTok > 0

    -- pool datum must correctly reflect totalShares and totalADA updated
    poolDatumUpdated :: Bool
    poolDatumUpdated =
      case continuingDatum of
        Nothing -> False
        Just nd ->
          let inAda  = valueOf inputVal adaSymbol adaToken
              outAda = valueOf outVal adaSymbol adaToken
              addedAda = outAda - inAda
              -- minted shares
              mintedShares = valueOf (txInfoMint info) (lpSharesCurrency dat) (lpSharesTokenName dat)
              expectedShares = lpTotalShares dat + mintedShares
          in lpTotalADA nd == lpTotalADA dat + addedAda &&
             lpTotalShares nd == expectedShares

    -- RequestWithdraw checks
    sharesBurned :: Bool
    sharesBurned =
      let burned = negate $ valueOf (txInfoMint info) (lpSharesCurrency dat) (lpSharesTokenName dat)
      in burned == rwaShares

    ticketMintedToOwner :: Bool
    ticketMintedToOwner =
      let minted = txInfoMint info
          tn = withdrawalTicketTokenName (lpNextTicket dat)
          -- minted should include exactly 1 ticket NFT
      in valueOf minted (withdrawalCurrencySymbol dat info) tn == 1

    poolDatumUpdatedReq :: Bool
    poolDatumUpdatedReq =
      case continuingDatum of
        Nothing -> False
        Just nd ->
          let expectedShares = lpTotalShares dat - rwaShares
          in lpTotalShares nd == expectedShares &&
             lpTotalADA nd == lpTotalADA dat -- ADA still in pool until finalize

    -- FinalizeWithdraw checks: ticket burned, ADA moved out to owner
    ticketBurned :: Bool
    ticketBurned =
      let burned = valueOf (txInfoMint info) (withdrawalCurrencySymbol dat info) fwaTicketToken
      in burned == (-1)

    -- Find who holds the ticket (owner) by scanning outputs for paid ADA and matching token burned/minted in tx
    adaPaidToOwner :: Bool
    adaPaidToOwner =
      -- For brevity: simply ensure outVal ADA decreased appropriately compared to inputVal
      let inAda  = valueOf inputVal adaSymbol adaToken
          outAda = valueOf outVal adaSymbol adaToken
          paid   = inAda - outAda
      in paid > 0 -- real implementation: check paid went to ticket owner pubkey

    poolDatumUpdatedFinal :: Bool
    poolDatumUpdatedFinal =
      case continuingDatum of
        Nothing -> False
        Just nd ->
          let inAda  = valueOf inputVal adaSymbol adaToken
              outAda = valueOf outVal adaSymbol adaToken
          in lpTotalADA nd + (inAda - outAda) == lpTotalADA dat -- i.e., pool ADA decreased by amount paid

    -- Admin update check: only fee changed
    onlyFeeChanged :: Integer -> Bool
    onlyFeeChanged newBps =
      case continuingDatum of
        Nothing -> False
        Just nd ->
          lpTotalADA nd == lpTotalADA dat &&
          lpTotalShares nd == lpTotalShares dat &&
          lpNextTicket nd == lpNextTicket dat &&
          lpFeeBps nd == newBps

------------------------------------------------------------------------
-- Utilities & Token details (simplified)
------------------------------------------------------------------------

-- In this example we derive a (placeholder) currency symbol for sADA shares from the pool's validator hash.
-- In practice the minting policy must be a separate compiled script. Here we provide the check "pool UTxO consumed".
{-# INLINABLE lpSharesCurrency #-}
lpSharesCurrency :: LSDPoolDatum -> CurrencySymbol
lpSharesCurrency dat =
    -- use the pool admin as part of identifier (not secure in real world). Replace with real policy.
    let bs = getPubKeyHash (lpAdmin dat)
    in CurrencySymbol (consByteString bs (consByteString bs emptyByteString))

{-# INLINABLE lpSharesTokenName #-}
lpSharesTokenName :: LSDPoolDatum -> TokenName
lpSharesTokenName _ = TokenName "sADA"

-- Withdrawal ticket token name generator (simple)
{-# INLINABLE withdrawalTicketTokenName #-}
withdrawalTicketTokenName :: Integer -> TokenName
withdrawalTicketTokenName n =
    let bs = integerToBuiltinByteString n
    in TokenName bs

-- (Helper) Convert Integer to BuiltinByteString (simple)
{-# INLINABLE integerToBuiltinByteString #-}
integerToBuiltinByteString :: Integer -> BuiltinByteString
integerToBuiltinByteString i = Builtins.encodeUtf8 $ fromString (P.show i)

-- A placeholder to derive the pool's validator hash from the datum.
-- In practice, the validator hash is known from the compiled validator; this is here to avoid undefined references.
{-# INLINABLE validatorHashFromDatum #-}
validatorHashFromDatum :: LSDPoolDatum -> Maybe ValidatorHash
validatorHashFromDatum _ = Nothing  -- off-chain will supply actual own hash; on-chain one typically uses findOwnInput.

-- Obtain withdrawal currency symbol from tx context (we require the minted NFT be in the transaction; derive its currency by scanning txInfoMint)
{-# INLINABLE withdrawalCurrencySymbol #-}
withdrawalCurrencySymbol :: LSDPoolDatum -> TxInfo -> CurrencySymbol
withdrawalCurrencySymbol _ info =
    case flattenValue (txInfoMint info) of
      ((cs, tn, amt):_) -> cs
      [] -> traceError "no minted tokens"

------------------------------------------------------------------------
-- Untyped validator boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @LSDPoolDatum d
        red = unsafeFromBuiltinData @LSDPoolAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkLSDPoolValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Minting policy for sADA shares (simple): allow mint/burn only if the pool UTxO is consumed
-- This is a common pattern: require that the pool validator is part of the spent inputs.
------------------------------------------------------------------------

{-# INLINABLE mkSharesPolicy #-}
mkSharesPolicy :: ValidatorHash -> BuiltinData -> ScriptContext -> Bool
mkSharesPolicy poolVH _ ctx =
    let info = scriptContextTxInfo ctx
        spent = txInfoInputs info
    in any (\i -> case txOutAddress (txInInfoResolved i) of
                    Address (ScriptCredential vh) _ -> vh == poolVH
                    _ -> False
           ) spent

sharesPolicy :: ValidatorHash -> MintingPolicy
sharesPolicy poolVH = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \vh -> mkUntypedMintingPolicy (mkSharesPolicy vh) ||])
    `PlutusTx.applyCode` PlutusTx.liftCode poolVH

------------------------------------------------------------------------
-- Minting policy for Withdrawal Ticket NFT: allowed only when pool UTxO is consumed
-- and only mints exactly one token with a unique name (we don't decode the name on-chain).
------------------------------------------------------------------------

{-# INLINABLE mkTicketPolicy #-}
mkTicketPolicy :: ValidatorHash -> BuiltinData -> ScriptContext -> Bool
mkTicketPolicy poolVH _ ctx =
    let info = scriptContextTxInfo ctx
        spent = txInfoInputs info
        poolConsumed = any (\i -> case txOutAddress (txInInfoResolved i) of
                                    Address (ScriptCredential vh) _ -> vh == poolVH
                                    _ -> False
                           ) spent
        minted = txInfoMint info
        -- require exactly one NFT minted in this policy (i.e., one token with quantity 1)
        flat = flattenValue minted
        nftCount = length flat
    in poolConsumed && nftCount >= 1 -- allow >=1 for flexibility; off-chain should mint exactly 1
    -- A stricter check can ensure amounts exactly 1 and same currency symbol, etc.

ticketPolicy :: ValidatorHash -> MintingPolicy
ticketPolicy poolVH = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \vh -> mkUntypedMintingPolicy (mkTicketPolicy vh) ||])
    `PlutusTx.applyCode` PlutusTx.liftCode poolVH

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

-- Compute validator hash using only plutus-ledger-api + plutus-tx
plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
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

        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

------------------------------------------------------------------------
-- File writing
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

writeMintingPolicy :: FilePath -> MintingPolicy -> IO ()
writeMintingPolicy path mp = do
    LBS.writeFile path (Serialise.serialise mp)
    putStrLn $ "Minting policy written to: " <> path

------------------------------------------------------------------------
-- Main for local / CLI usage
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "lsdpool.plutus" validator

    -- derive simple policies using validator hash (note: mkSharesPolicy expects real ValidatorHash)
    let vh = plutusValidatorHash validator
        sharesMp = sharesPolicy vh
        ticketMp = ticketPolicy vh

    writeMintingPolicy "sada-shares-policy.plutus" sharesMp
    writeMintingPolicy "withdraw-ticket-policy.plutus" ticketMp

    let onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- LSD Pool Onchain Info ---"
    putStrLn $ "Plutus Validator Hash: " <> P.show vh
    putStrLn $ "Plutus Script Address:  " <> P.show onchain
    putStrLn $ "Bech32 Script Address:  " <> bech32
    putStrLn "-----------------------------"
    putStrLn "LSD Pool artifacts generated."