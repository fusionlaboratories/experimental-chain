# Jusion Chain Proof-of-Concept

## Stratified System

Assuming that Qredo is running as DApp on top of Fusion, then there are at least two levels:

1. Fusion (untrusted, decentralized), and
    1. ZK smart contracts,
    2. Sending qrdo token transactions,
    3. Provide native tokens.
2. Qredo (trusted, federated)
    1. L1 Oracles,
    2. Accepting tokens from L1 and minting them as wrapped tokens on Fusion, and
    3. Releasing tokens to L1 by destroying wrapped tokens on Fusion.

## TODO

- [ ] Switch to using nix flakes.  There is a simple `flake.nix`, but I am not using it yet.
  - [ ] direnv integration.  Need to install and set up direnv.
- [ ] Basic ZK DSL
- [ ] Basic interface for an L1 Oracle
- [ ] What is sufficient to uniquely determine a transaction?  Block Height and Transaction Position?  Or Transaction Hash?  Or the triple?
