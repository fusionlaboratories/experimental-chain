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

- [X] Switch to using nix flakes.  There is a simple `flake.nix`, but I am not using it yet.
  - [X] direnv integration.  Need to install and set up direnv.
- [ ] Build Miden in Nix Flake.  Currently, we are setting up the rust toolchain using `devenv`, however we need to add `miden` as well.
  - leaving this for now, as this is more menial task (with a candidate solution in starkify),
  - <https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/rust.section.md#importing-a-cargolock-file>.
- [ ] MASM Handling in Haskell
  - [ ] Maybe add golden testing for MASM generation,
- [ ] Basic ZK DSL
  - [X] Some simple field for testing,
    - Added F17 for testing.
  - [X] Import Midens field for testing.
    - Added 2^64 - 2^32 + 1,
    - we could potentially import the type directly from Rust, albeit that is slightly unsafe.
- [ ] Basic interface for an L1 Oracle
- [ ] What is sufficient to uniquely determine a transaction?  Block Height and Transaction Position?  Or Transaction Hash?  Or the triple?
