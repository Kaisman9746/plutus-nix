Below is a **full tutorial for Treasury Governance Smart Contract**, written in the **exact same style, structure, and tone** as the Vesting.hs tutorial you provided — including **Table of Contents, section formatting, emojis, and glossary**.

---

# 🧾 Detailed Tutorial: Understanding and Using `TreasuryValidator.hs`

This tutorial covers the Treasury Governance smart contract, highlighting its purpose, essential imports, proposal logic, governance operations, and real-world usage scenarios. This module is the core of a decentralized treasury system for community governance on Plutus.

---

# 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃 Data Structures](#2-data-structures)
3. [🧠 Core Validator Logic](#3-core-validator-logic)
4. [⚙ Validator Script Compilation](#4-validator-script-compilation)
5. [🔧 Helper Functions](#5-helper-functions)
6. [🧪 Practical Usage Example](#6-practical-usage-example)
7. [🧷 Testing Strategy](#7-testing-strategy)
8. [✅ Best Practices](#8-best-practices)
9. [📘 Glossary of Terms](#9-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus API Modules

* **Plutus.V2.Ledger.Api**
  Provides all core Plutus types used in script validation (validators, datums, redeemers, TxInfo, Value, CurrencySymbol, etc.).

* **Plutus.V2.Ledger.Contexts**
  Supplies helper functions for reading transaction details such as signatures, inputs, outputs, and paid values.

* **Plutus.V1.Ledger.Interval**
  Used for deadline checking in proposals (e.g., *voting deadline*, *execution deadline*).

### Ledger Value Modules

* **Plutus.V1.Ledger.Value**
  Gives access to functions like `valueOf`, `adaSymbol`, and `adaToken` to validate token/ADA distributions.

### Serialization Modules

* **Codec.Serialise**
  Converts validators into `.plutus` serialized scripts ready for deployment.

### Cardano API Modules

* **Cardano.Api / Shelley**
  Used to produce Bech32 script addresses.

### Utility Modules

* **PlutusTx & PlutusTx.Prelude**
  Required for on-chain compilation, data encoding, and Plutus-optimized operations.

---

## 2. 🗃 Data Structures

### **Split**

A simple structure defining a payment division:

* `spRecipient` — beneficiary of the split.
* `spShare` — numeric share this recipient should receive.

### **Treasury**

Represents the DAO’s configuration:

* `trCommittee` — list of committee members with signing authority.
* `trQuorumPerc` — percentage of votes required for decision approval.
* `trMinProposal` — minimum ADA amount for a proposal to be valid.

### **Proposal**

A complete governance proposal:

* `pId` — unique identifier.
* `pProposer` — committee member who created it.
* `pRecipient` — wallet to receive funds if proposal passes.
* `pAmount` — requested ADA amount.
* `pPurpose` — reason/purpose for the request.
* `pDeadline` — deadline for voting.
* `pYes`, `pNo` — recorded votes.
* `pExecuted` — whether proposal has already been executed.
* `pEarmarkCS`, `pEarmarkTN` — optional token earmark restrictions.

### **TreasuryDatum**

Holds:

* governance parameters (`Treasury`)
* list of all created proposals

### **TreasuryAction (Redeemer)**

The contract supports four operations:

1. **Donate** — add funds to treasury.
2. **CreateProposal Proposal** — submit a new proposal.
3. **Vote Bool proposalId** — cast a yes/no vote.
4. **ExecuteProposal proposalId** — execute after voting period if passed.

---

## 3. 🧠 Core Validator Logic

### `mkValidator`

This is the heart of the governance system. It validates all treasury actions.

---

### **1. Donate**

Checks:

* Contract balance must increase.
  (Your code uses a placeholder `donationIncreases = True`, but normally this enforces value checks.)

---

### **2. CreateProposal**

Validates:

* Proposer is a committee member (`txSignedBy`).
* Proposal amount ≥ minimum threshold.
* Proposal ID is not duplicated.

This prevents spam proposals and ensures authorized participation.

---

### **3. Vote**

Ensures:

* Proposal exists.
* Voting period has NOT expired (`contains pDeadline`).
* Proposal has not been executed yet.

Votes are accepted regardless of yes/no — only deadline matters.

---

### **4. ExecuteProposal**

Enforces the full governance criteria:

* Voting must be finished (`after pDeadline`).
* Proposal must not be executed before.
* Quorum AND majority must be met.
* Earmark token requirements must be satisfied.
* Recipient must be paid the required ADA amount.

This ensures secure treasury disbursement based on democratic voting.

---

## 4. ⚙ Validator Script Compilation

### `mkValidatorUntyped`

Wraps the typed validator to handle raw `BuiltinData` for on-chain execution.

### `validator`

Compiles the script via Template Haskell:

```haskell
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])
```

This produces the Plutus Core script used on Cardano nodes.

---

## 5. 🔧 Helper Functions

### Proposal Helpers

* **findProposal** — searches for a proposal by ID in the treasury list.
* **totalSupplyOfEarmark** — verifies token earmarks for more advanced proposals.

### Script Address / Hash Helpers

* **plutusValidatorHash**
* **plutusScriptAddress**
* **toBech32ScriptAddress**

These functions generate:

* Plutus-level validator hash
* Ledger-compatible script address
* Human-readable Bech32 script address

### File Output

* **writeValidator** — serializes and writes `.plutus` script files to disk.

---

## 6. 🧪 Practical Usage Example

```haskell
main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    -- Write compiled validator file
    writeValidator "religion_treasury.plutus" validator

    -- Get script hash and addresses
    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "--- Religion Treasury Validator Info ---"
    print vh
    print onchain
    putStrLn bech32
```

This produces:

* The serialized `.plutus` script
* Script hash
* On-chain Plutus address
* Bech32 Cardano address

---

## 7. 🧷 Testing Strategy

To fully test the treasury governance system:

### **Donation Tests**

* Ensure contract UTxO increases.

### **Proposal Creation Tests**

* Proposer *not* in committee → reject.
* Duplicate proposal ID → reject.
* Amount too low → reject.
* Success case → accept.

### **Voting Tests**

* After deadline → reject.
* Already executed → reject.

### **Execution Tests**

* Before deadline → reject.
* Quorum not met → reject.
* Majority not met → reject.
* Missing earmark → reject.
* Recipient not paid → reject.

### **Integration Tests**

Simulate full lifecycle:

1. Donate
2. Create proposal
3. Vote yes
4. Execute

---

## 8. ✅ Best Practices

* Always enforce strict proposal ID uniqueness.
* Log clear trace messages for debugging.
* Check voting math carefully (quorum + majority).
* Include token earmark logic for advanced treasury operations.
* Use structured data types to ensure safe updates to proposal lists.
* Always verify payment to proposal recipients.

---

## 9. 📘 Glossary of Terms

| Term          | Definition                                                       |
| ------------- | ---------------------------------------------------------------- |
| **Treasury**  | Shared pool of funds controlled by a DAO/community.              |
| **Proposal**  | A request for treasury funds.                                    |
| **Committee** | Authorized members allowed to submit proposals.                  |
| **Quorum**    | Required participation percentage for decisions to be valid.     |
| **Majority**  | More `yes` votes than `no` votes.                                |
| **Datum**     | State stored on-chain for the contract.                          |
| **Redeemer**  | The action being performed (Donate, Vote, etc.).                 |
| **Earmark**   | Restriction tying spending to a specific token or asset.         |
| **Bech32**    | Human-readable Cardano address format.                           |
| **Validator** | Smart contract that checks whether treasury actions are allowed. |

---

 
