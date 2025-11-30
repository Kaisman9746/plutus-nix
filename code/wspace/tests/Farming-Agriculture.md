Absolutely — here is a **full, clean, structured tutorial** *matching the exact style and format* of the Vesting.hs tutorial you provided, but written specifically for **your Farming Smart Contract** (FarmDatum, FarmRedeemer, validator, minting policy, serialization, etc.).

I kept the same tone, emoji style, formatting style, headings, and flow — just applied to your contract.

---

# 🌾 Detailed Tutorial: Understanding and Using Your Farming Smart Contract

This tutorial explains your farming contract module in a structured and beginner-friendly way. It covers imports, data structures, validator logic, minting logic, serialization, and best practices — all matching the structure of your previous Vesting.hs tutorial.

---

# 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃 Data Structures](#2-data-structures)
3. [🧠 Core Validator Logic](#3-core-validator-logic)
4. [⚙ Wrapped Validator Script](#4-wrapped-validator-script)
5. [💰 Minting Policy Logic](#5-minting-policy-logic)
6. [🧪 Serialization Helpers](#6-serialization-helpers)
7. [🔧 Practical Usage Examples](#7-practical-usage-examples)
8. [🧷 Testing Strategy](#8-testing-strategy)
9. [✅ Best Practices](#9-best-practices)
10. [📘 Glossary of Terms](#10-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus API Modules

* **Plutus.V2.Ledger.Api**
  Provides core on-chain types such as `PubKeyHash`, `POSIXTime`, `Validator`, `MintingPolicy`, and `ScriptContext`.

* **Plutus.V2.Ledger.Contexts**
  Gives access to transaction info and signature checks, particularly `txSignedBy`.

* **Plutus.V1.Ledger.Interval**
  Used for time-range validation with `from`, `to`, and `contains`.

### Template Haskell & Prelude

* **PlutusTx**
  Enables compiling your on-chain code to Plutus Core.

* **PlutusTx.Prelude**
  Replaces Haskell’s Prelude in smart contracts to ensure deterministic behavior.

* **Haskell Prelude**
  Only used for off-chain utilities (File IO, printing, etc.).

### Serialization & ByteString

* **Codec.Serialise**
  Used to serialize validators and minting policies for writing `.plutus` files.

---

## 2. 🗃 Data Structures

### **FarmDatum**

Represents all necessary information for a farming investment cycle:

* `fdFarmer` — farmer’s public key hash
* `fdCoop` — cooperative’s public key hash
* `fdInvestment` — amount invested
* `fdProfitShare` — profit-share percentage
* `fdStart` — allowed start time for harvest
* `fdExpiry` — end of valid harvest window

This datum controls when funds can be harvested or refunded.

### **FarmRedeemer**

Your contract supports two actions:

* **Harvest** — used by the cooperative to collect harvest proceeds
* **Refund** — used by the farmer to reclaim funds after harvest window ends

---

## 3. 🧠 Core Validator Logic

### **farmValidator**

The validator enforces **time-based rules** and **signature checks** depending on the redeemer:

#### **Harvest Condition**

* Must happen **after or at the start time**
* Must happen **before or at the expiry time**

This ensures the cooperative can only harvest **within the valid farming window**.

#### **Refund Condition**

* Refund allowed **only after expiry**
* Must be signed by the **farmer**

This protects the cooperative’s investment during the active farm period.

---

## 4. ⚙ Wrapped Validator Script

### **mkWrapped**

Wraps the typed validator (`farmValidator`) inside untyped Plutus structures using `BuiltinData`.

Plutus requires this untyped wrapper for script execution on-chain.

### **validator**

Compiles the validator into an actual deployable Plutus script using:

```haskell
mkValidatorScript $$(PlutusTx.compile [|| mkWrapped ||])
```

---

## 5. 💰 Minting Policy Logic

### **mkCoopMintPolicy**

This minting policy ensures that:

* Only the cooperative (`PubKeyHash`) can mint or burn tokens associated with farming cycles.

Uses:

* `txSignedBy info coop`

### **mkUntyped**

Converts the typed minting policy into untyped form.

### **policy**

Final minting policy script generator:

```haskell
policy :: PubKeyHash -> MintingPolicy
```

Used when deploying cooperative-controlled farm tokens.

---

## 6. 🧪 Serialization Helpers

### **writeValidator**

Serializes the validator script into `.plutus` format for deployment.

### **writePolicy**

Serializes the minting policy into `.plutus` format.

These files are essential for:

* Running transactions via cardano-cli
* Using off-chain frameworks
* Interacting with dApps

---

## 7. 🔧 Practical Usage Examples

Below are examples matching your script utilities.

### Save the validator as a file

```haskell
writeValidator "farm-validator.plutus" validator
```

### Save the cooperative minting policy

```haskell
writePolicy "coop-policy.plutus" (policy coopPKH)
```

### Print success message (already in your main)

```
Farming contract compiled successfully.
```

---

## 8. 🧷 Testing Strategy

Recommended tests:

### For `Harvest`

* Attempt harvest **before start** → should fail
* Attempt harvest **after expiry** → should fail
* Attempt harvest **within window** → should succeed

### For `Refund`

* Refund **before expiry** → must fail
* Refund **after expiry** but **without farmer signature** → must fail
* Refund **after expiry** with farmer signature → must succeed

### For Minting Policy

* Minting without cooperative signature → should fail
* Minting with correct signature → should succeed

---

## 9. ✅ Best Practices

* Always verify **time ranges** carefully.
* Use meaningful `traceIfFalse` messages for debugging.
* Encode all economically significant parameters in datum.
* Test edge cases around `start`, `expiry`, and signature verification.
* Keep minting policies simple and signature-based when possible for security.

---

## 10. 📘 Glossary of Terms

| Term               | Definition                                                        |
| ------------------ | ----------------------------------------------------------------- |
| **FarmDatum**      | All on-chain data representing a farming investment contract.     |
| **Redeemer**       | Specifies which action (Harvest/Refund) is being taken on a UTxO. |
| **PubKeyHash**     | Hash of a user’s public key used for authentication.              |
| **POSIXTime**      | Plutus-compatible time representation.                            |
| **txSignedBy**     | Checks if a public key signed a transaction.                      |
| **Minting Policy** | Rule controlling who can mint or burn tokens.                     |
| **Validator**      | On-chain logic that determines whether spending is allowed.       |
| **Interval**       | Time window where a transaction must occur.                       |
| **BuiltinData**    | Low-level Plutus data type required for on-chain execution.       |
| **Serialization**  | Converting scripts into `.plutus` files for deployment.           |

---

 