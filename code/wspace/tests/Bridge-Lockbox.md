Below is a **full, polished tutorial** for your **Lockbox Custodial Smart Contract**, modeled **exactly in the same structure and style** as the Vesting tutorial you provided — including:

✅ Full sections
✅ Table of contents
✅ Glossary
✅ Clear explanations
✅ Accurate interpretation of your code
❗ *No content was altered in your contract — only explained.*

---

# 🧾 Detailed Tutorial: Understanding and Using `Lockbox.hs`

A complete guide to your custodial bridge lockbox smart contract.

This tutorial explains the **Lockbox validator**, its **datum**, **redeemer**, **logic**, **addressing**, and **deployment details**.

---

# 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Data Structures](#2-data-structures)
3. [🧠 Core Validator Logic](#3-core-validator-logic)
4. [🔐 NFT & Asset Handling](#4-nft--asset-handling)
5. [⚙️ Validator Script Compilation](#5-validator-script-compilation)
6. [🔧 Helper Functions](#6-helper-functions)
7. [🧪 Practical Usage Examples](#7-practical-usage-examples)
8. [🧷 Testing Strategy](#8-testing-strategy)
9. [✅ Best Practices](#9-best-practices)
10. [📘 Glossary of Terms](#10-glossary-of-terms)

---

# 1. 📦 Imports Overview

### Plutus Ledger Modules

The contract uses:

* **Plutus.V2.Ledger.Api**
  Provides core types: `Value`, `PubKeyHash`, `ScriptContext`, `TxOut`, `Validator`, etc.

* **Plutus.V2.Ledger.Contexts**
  Used for:

  * `txSignedBy`
  * `findOwnInput`
  * accessing input/output state

* **Plutus.V1.Ledger.Interval**
  Provides time interval functions (not used in this validator specifically, but imported for compatibility).

* **Plutus.V1.Ledger.Value**
  Used for extracting asset quantities:

  ```
  valueOf v cs tn
  ```

### PlutusTx and Prelude

* Serialisation, compilation, and built-in data handling.
* `PlutusTx.Prelude` is used instead of normal Haskell `Prelude`.

### Cardano.API

Used strictly for generating:

* Script address (Bech32)
* Off–chain serialization
* Hashing the script for CLI use

---

# 2. 🗃️ Data Structures

Your contract defines two key types.

---

## **LockboxDatum**

This describes the **stateful information stored with each UTxO**:

| Field             | Meaning                                            |
| ----------------- | -------------------------------------------------- |
| `lbUser`          | The user depositing the asset                      |
| `lbAssetCurrency` | Currency symbol of the locked token                |
| `lbAssetToken`    | Token name of the locked token                     |
| `lbAmount`        | Amount locked                                      |
| `lbCustodian`     | The bridge custodian permitted to authorize unlock |
| `lbOracle`        | Oracle PKH required to attest burn event           |
| `lbLockboxNFTCS`  | Currency symbol of lockbox NFT (identity marker)   |
| `lbLockboxNFTTN`  | Token name of lockbox NFT                          |
| `lbFee`           | Fee paid to custodian upon unlock                  |
| `lbExpectedBurn`  | Expected burn-proof hash (sha256-like)             |

The datum is converted to on-chain data using:

```
PlutusTx.unstableMakeIsData ''LockboxDatum
```

---

## **LockboxAction (Redeemer)**

Represents the user action:

* `LLock` — used when locking the asset
* `LUnlock burnHash` — used when unlocking, providing oracle burn proof

---

# 3. 🧠 Core Validator Logic

The validator enforces **two phases**:

---

## **LLock — Locking Phase**

Checks:

1. **Lockbox NFT is present**
2. **The locked asset amount ≥ expected amount**
3. **Transaction is signed by the depositing user**

Purpose:
Prevents unauthorized initial deposits and ensures correct asset deposit.

---

## **LUnlock — Unlocking Phase**

Checks:

1. Lockbox NFT must still be in input (correct UTxO)
2. Custodian signature is required
3. Oracle signature is required
4. Provided burn hash must exactly match expected hash
5. User must receive the originally locked asset in the same quantity
6. Custodian must receive the required fee in ADA

Purpose:
Ensures **the asset on the foreign chain is proven burned**, and **custodian/ oracle approve unlock**, preventing fraudulent unlocks.

---

# 4. 🔐 NFT & Asset Handling

The lockbox UTxO is uniquely identified by its **NFT**.

### `scriptInputContainsNFT`

Ensures the UTxO being consumed belongs to the lockbox.

### `valueLockedInInput`

Reads how much of the specific asset is currently locked.

### `valuePaidTo` / `adaPaidTo`

Used to validate:

* asset returned to user
* ADA fee paid to custodian

---

# 5. ⚙️ Validator Script Compilation

The script is compiled using:

```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])
```

The untyped wrapper ensures the contract can run in Plutus V2.

---

# 6. 🔧 Helper Functions

### **plutusValidatorHash**

Computes an on-chain validator hash *using Plutus only*.

### **plutusScriptAddress**

Builds the **Plutus on-chain Address (Credential)**.

### **toBech32ScriptAddress**

Generates a **Bech32 script address** using the Cardano API — crucial for:

* CLI usage
* wallet integration
* off-chain code

### **writeValidator**

Writes the script (`.plutus` file) to disk.

---

# 7. 🧪 Practical Usage Examples

### **1. Generate the validator**

```
cabal run lockbox
```

Output includes:

* validator hash
* Plutus script address
* Bech32 script address

### **2. Export the `.plutus` file**

```
lockbox-validator.plutus
```

### **3. Construct datum JSON off-chain**

Your datum must be encoded and attached to the UTxO.

### **4. Use Redeemers**

* For locking: `LLock`
* For unlocking: `LUnlock "burn-proof-hash"`

---

# 8. 🧷 Testing Strategy

### **Lock Phase Tests**

* Verify deposit requires user signature
* Ensure locked amount must match
* Ensure NFT must be attached

### **Unlock Phase Tests**

* Missing custodian signature → fail
* Missing oracle signature → fail
* Wrong burn hash → fail
* Wrong NFT UTxO → fail
* User receives asset fully → pass/fail
* Custodian fee is correct → pass/fail

Testing should cover edge cases and invalid scenarios.

---

# 9. ✅ Best Practices

* Use **unique NFTs per lockbox** to prevent fraud.
* Always validate **signatures + asset correctness** together.
* For bridge logic, always verify a **cryptographic burn proof**.
* Maintain consistent fee logic off-chain and on-chain.
* Add trace messages for easier debugging.

---

# 10. 📘 Glossary of Terms

| Term                         | Meaning                                                         |
| ---------------------------- | --------------------------------------------------------------- |
| **Lockbox**                  | A custodial UTxO holding tokens until proof of burn is provided |
| **Datum**                    | On-chain metadata stored with a UTxO                            |
| **Redeemer**                 | Action data passed when spending a script UTxO                  |
| **PubKeyHash**               | Hash of user/custodian/oracle public key                        |
| **Oracle**                   | Trusted signer proving external events (like burn)              |
| **Custodian**                | Bridge controller who authorizes unlocks                        |
| **Burn Proof**               | Hash verifying asset burn on foreign chain                      |
| **NFT (Non-Fungible Token)** | Unique identifier for each lockbox UTxO                         |
| **ScriptContext**            | Full transaction information provided to validator              |
| **Value**                    | Multi-asset container in Cardano                                |
| **Bech32**                   | Human-readable address format                                   |
| **Validator**                | Smart contract enforcing transaction rules                      |

---

 