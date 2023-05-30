# Jusion Chain Proof-of-Concept

## Development Shell

Unfortunately, Nix Flakes are still an experimental feature, therefore they need to be explicitly enabled.

- [Flakes - NixOS Wiki](https://nixos.wiki/wiki/Flakes)

```sh
nix develop --impure
```

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
