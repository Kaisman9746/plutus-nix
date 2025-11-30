Below is a **full, structured, beautifully formatted tutorial** for your **Limit Order Smart Contract**, modeled exactly after the Vesting.hs tutorial you provided — **same style, same structure, same tone**, but adapted to your codebase.

Everything is tailored **specifically to your smart contract**.

---

# 🧾 Detailed Tutorial: Understanding and Using Your Limit-Order Smart Contract

This tutorial explains the purpose, structure, and usage of your `limit-order.plutus` smart contract. It describes the validator logic, datum and redeemer formats, price-check rules, serialization utilities, and deployment-ready script generation.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃 Data Structures](#2-data-structures)
3. [🧠 Core Validator Logic](#3-core-validator-logic)
4. [📈 Price-Check Helper](#4-price-check-helper)
5. [⚙ Validator Script Compilation](#5-validator-script-compilation)
6. [🔧 Script Hashing & Address Generation](#6-script-hashing--address-generation)
7. [💾 File Output](#7-file-output)
8. [🧪 Practical Usage Example](#8-practical-usage-example)
9. [🧷 Testing Strategy](#9-testing-strategy)
10. [✅ Best Practices](#10-best-practices)
11. [📘 Glossary of Terms](#11-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus Core Modules

* **Plutus.V2.Ledger.Api**
  Provides key on-chain types such as `PubKeyHash`, `CurrencySymbol`, `Validator`, `ScriptContext`, and `TxOut`.

* **Plutus.V2.Ledger.Contexts**
  Includes validators for signatures and access to transaction inputs/outputs.

* **PlutusTx / PlutusTx.Prelude**
  Enables compilation of logic to on-chain Plutus Core and supplies low-level builtin operations.

* **Plutus.V1.Ledger.Interval**
  Provides interval computation tools (though lightly used here).

### Serialization and Cardano API

* **Codec.Serialise**
  Serializes Plutus validators to CBOR for `.plutus` script output.

* **Cardano.Api / Cardano.Api.Shelley**
  Used to compute script hashes and Bech32 addresses (Testnet/Mainnet compatible).

---

## 2. 🗃 Data Structures

### 🧾 OrderDatum

This defines the structure stored *inside* the UTxO being traded:

| Field         | Meaning                                               |
| ------------- | ----------------------------------------------------- |
| `odOwner`     | PubKeyHash of the order owner (only they can cancel). |
| `odBase`      | Currency symbol of the base asset.                    |
| `odQuote`     | Currency symbol of the quote asset.                   |
| `odSide`      | `True = BUY`, `False = SELL`.                         |
| `odLimitNum`  | Numerator of limit price.                             |
| `odLimitDen`  | Denominator of limit price.                           |
| `odQty`       | Original order quantity.                              |
| `odRemaining` | Remaining amount left to fill.                        |

### 🏷 OrderAction (Redeemer)

Used by takers or the owner to interact with the order.

* **Fill amount execNum execDen**

  * `amount`: how much of the order the taker wants to fill
  * `execNum/execDen`: execution price offered by taker

* **Cancel**

  * Only allowed if the owner signs the transaction.

---

## 3. 🧠 Core Validator Logic

### mkValidator

The validator enforces rules depending on whether the user is **filling** or **cancelling** the order.

#### When the order is **FILLED**:

The validator checks:

1. **Remaining must be sufficient**

   ```
   amount <= odRemaining dat
   ```

2. **Execution price must satisfy limit price**
   Uses the priceOK function.

3. **The remaining amount must be updated correctly**
   The continuing output must have

   ```
   newRemaining = oldRemaining - amount
   ```

#### When the order is **CANCELLED**:

The validator ensures:

* **Only the owner can cancel**

  ```
  txSignedBy info (odOwner dat)
  ```

---

## 4. 📈 Price-Check Helper

### priceOK

Ensures the execution price obeys the limit price.

* **For BUY orders** — taker price must be **<= limit price**
* **For SELL orders** — taker price must be **>= limit price**

This ensures the order is filled only on acceptable price terms.

---

## 5. ⚙ Validator Script Compilation

The contract uses Template Haskell to compile `mkValidator` into a `Validator`:

```
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])
```

`mkValidatorUntyped` converts raw `BuiltinData` into typed Haskell structures and calls the typed validator.

---

## 6. 🔧 Script Hashing & Address Generation

Your contract includes helper functions to:

### ✔ Compute the validator hash

Used to identify the script on-chain.

### ✔ Create a script address

This uses:

```
makeShelleyAddressInEra
```

It produces a **Bech32**-encoded address (Testnet / Mainnet).

---

## 7. 💾 File Output

### writeValidator

Writes the fully compiled `.plutus` script:

```
writeValidator "limit-order.plutus" validator
```

This file can be used with:

* cardano-cli
* MeshJS
* Lucid
* Kupo / Ogmios indexing pipelines

---

## 8. 🧪 Practical Usage Example

Below explains what your `main` function does.

### 1️⃣ Compile the contract

```
writeValidator "limit-order.plutus" validator
```

### 2️⃣ Print useful information

* Validator hash
* Plutus script address
* Bech32 address

### 3️⃣ Confirm generation success

```
Limit order validator generated successfully.
```

---

## 9. 🧷 Testing Strategy

To test your limit order contract, simulate:

### Price validation

* BUY order vs lower/higher execution price
* SELL order vs higher/lower execution price

### Remaining amount updates

* Correct decrements
* Prevent negative remaining

### Cancelation attempts

* Owner signature present → should succeed
* Signature missing or wrong → should fail

### UTxO structure

* Must always produce exactly **one continuing output** except in cancellation.

---

## 10. ✅ Best Practices

* Use trace messages for debugging:

  * "remaining insufficient"
  * "bad price"
  * "owner sig missing"
  * etc.

* Always check continuing outputs carefully to maintain deterministic validator behavior.

* When integrating off-chain:

  * Always recalculate the remaining quantity locally before submission.
  * Ensure execution price follows your limit rules.

---

## 11. 📘 Glossary of Terms

| Term                  | Definition                                                        |
| --------------------- | ----------------------------------------------------------------- |
| **Limit Order**       | An order allowing trade only at or better than a specified price. |
| **Datum**             | On-chain metadata stored with a script-address UTxO.              |
| **Redeemer**          | Instruction/data provided when spending a script UTxO.            |
| **ScriptContext**     | Full transaction context passed to validator.                     |
| **txSignedBy**        | Confirms a given PubKeyHash signed the transaction.               |
| **CurrencySymbol**    | 56-byte ID representing a token or native asset.                  |
| **ValidatorHash**     | Cryptographic identifier for a Plutus script.                     |
| **Bech32 Address**    | Human-readable address format used on Cardano.                    |
| **Continuing Output** | Output that remains locked at the same script address.            |
| **Execution Price**   | Taker-specified price used to fill the order.                     |

---

 