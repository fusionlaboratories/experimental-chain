# Jusion Chain Proof-of-Concept

## Verifiable Oracle

How does verifiable Oracle observe transactions on the L1 chain?  Similarly to a Kachina Transition Funciton, but the Oracle performs the following Public Oracle Queries:

16 field elements:

- verify that a particular block exists (1 + 1 + 4 field elements):
  - for particular L1 (1 field element, ignored because we assume ETH),
  - with particular block height (1 field element), and
  - with particular hash (4 field element, 32 bytes).
- verify that a particular transaction exists within that block (1 + 4 field elements)
  - transaction position (1 field element),
  - transaction hash (4 field element).
- perform the transaction on L2 (4 + 1 field elements):
  - destination address of the transaction (4 field element),
  - amount of wrapped tokens (1 field element).

Note, we represent Words (32 bytes) as 4 field elements.

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

- [ ] Field Representation
  - [ ] Plug in Rust Representation of Field
  - [ ] Use Word64 as a representation instead of Integer
- [ ] ZK DSL
  - [ ] Basic AST based on MASM
  - [ ] Consider using Ed's Bound for that DSL
- [ ] MASM Generation
  - [X] Reuse MASM code from Starkify
  - [ ] Consider adding MASM parser
- [ ] Basic infrastructure for golden testing
  - [X] Detect tests automatically, so I can avoid all the compilation overhead
    There are some various utilities in [tasty-golden/Various Utilities](https://hackage.haskell.org/package/tasty-golden-2.3.5/docs/Test-Tasty-Golden.html#g:3), which should be helpful in detecting all the test cases.
- [ ] Miden Handling in Haskell
  - [-] Add Data Structure for Miden Input
  - [X] Fix miden to emit exit codes properly.
  - [ ] Write a custom driver for the miden vm (the miden CLI is pretty barebone)
  - [ ] Consider importing Miden VM Rust bindings to Haskell...
- [X] Basic interface for an L1 Oracle
- [ ] What is sufficient to uniquely determine a transaction?  Block Height and Transaction Position?  Or Transaction Hash?  Or the triple?

## Archived

Here are tasks that have been completed.

- [X] Some simple field for testing,
  - Added F17 for testing.
- [X] Import Midens field for testing.
  - Added 2^64 - 2^32 + 1,
  - we could potentially import the type directly from Rust, albeit that is slightly unsafe.
- [X] Switch to using nix flakes.  There is a simple `flake.nix`, but I am not using it yet.
  - [X] direnv integration.  Need to install and set up direnv.
- [X] Build Miden in Nix Flake.  Currently, we are setting up the rust toolchain using `devenv`, however we need to add `miden` as well.
  - leaving this for now, as this is more menial task (with a candidate solution in starkify),
  - <https://github.com/NixOS/nixpkgs/blob/master/doc/languages-frameworks/rust.section.md#importing-a-cargolock-file>.
