<script type="module">
import {
  Lucid,
  Blockfrost,
  Data,
  Constr
} from "https://unpkg.com/lucid-cardano@0.10.11/web/mod.js";


// -------------------------
// Setup Lucid
// -------------------------
const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    "YOUR_BLOCKFROST_KEY"
  ),
  "Preprod"
);


// -------------------------
// Script
// -------------------------
const SCRIPT_ADDRESS = "YOUR_SCRIPT_ADDRESS";

// paste EXACT CBOR HEX you generated
const SCRIPT_HEX = `YOUR_SCRIPT_CBORHEX`;

const script = {
  type: "PlutusV2",
  script: SCRIPT_HEX
};


// -------------------------
// Wallet Connection
// -------------------------
let connectedAddress = null;

async function connectWallet() {
  if (!window.cardano || !window.cardano.lace) {
    alert("Install Lace Wallet");
    return;
  }

  const api = await window.cardano.lace.enable();
  lucid.selectWallet(api);

  connectedAddress = await lucid.wallet.address();
  document.getElementById("status").innerText =
    "Connected: " + connectedAddress;
}


// -------------------------
// Lock ADA
// -------------------------
async function lockFunds() {
  const amountAda = parseFloat(document.getElementById("amount").value);
  const deadlineHours = parseInt(document.getElementById("hours").value);

  if (!amountAda || !deadlineHours) {
    alert("Enter valid inputs");
    return;
  }

  const { paymentCredential } = lucid.utils.getAddressDetails(connectedAddress);
  const senderPKH = paymentCredential.hash;

  const receiverPKH =
    document.getElementById("receiver").value.replace("0x", "");

  const amountLov = BigInt(amountAda * 1_000_000);

  const deadline =
    BigInt(Date.now() + deadlineHours * 3600000); // POSIX ms

  const datum = Data.to(
    new Constr(0, [
      senderPKH,
      receiverPKH,
      amountLov,
      deadline
    ])
  );

  const tx = await lucid
    .newTx()
    .payToContract(
      SCRIPT_ADDRESS,
      { inline: datum },
      { lovelace: amountLov }
    )
    .complete();

  const signed = await tx.sign().complete();
  const hash = await signed.submit();

  document.getElementById("status").innerText =
    "Locked Successfully! Tx: " + hash;
}


// -------------------------
// Claim (Receiver signs)
// -------------------------
async function claimFunds() {
  const utxos = await lucid.utxosAt(SCRIPT_ADDRESS);

  const tx = await lucid
    .newTx()
    .collectFrom(utxos, Data.to(new Constr(0, [])))
    .attachSpendingValidator(script)
    .complete();

  const signed = await tx.sign().complete();
  const hash = await signed.submit();

  document.getElementById("status").innerText =
    "Claim Successful: " + hash;
}


// -------------------------
// Refund (Sender signs after deadline)
// -------------------------
async function refundFunds() {
  const utxos = await lucid.utxosAt(SCRIPT_ADDRESS);

  const tx = await lucid
    .newTx()
    .collectFrom(utxos, Data.to(new Constr(1, [])))
    .attachSpendingValidator(script)
    .complete();

  const signed = await tx.sign().complete();
  const hash = await signed.submit();

  document.getElementById("status").innerText =
    "Refund Successful: " + hash;
}
</script>
