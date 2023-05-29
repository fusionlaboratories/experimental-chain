{
  description = "Proof of Concept for Jusion";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    devenv.url = "github:cachix/devenv";
    miden-vm.url = "github:qredek/miden-vm/add-exitcodes";
    miden-vm.flake = false;
  };

  outputs = inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.devenv.flakeModule
      ];

      debug = true;

      # systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      systems = [ "aarch64-darwin" ];

      perSystem = { config, self', inputs', pkgs, system, ... }: {
        # Per-system attributes can be defined here. The self' and inputs'
        # module parameters provide easy access to attributes of the same
        # system.

        _module.args.pkgs = import self.inputs.nixpkgs {
          inherit system;
          overlays = [(import self.inputs.rust-overlay)];
        };

        # Latest Rust toolchain
        packages.rust = pkgs.rust-bin.stable.latest.default;

        # Miden VM
        packages.miden = pkgs.rustPlatform.buildRustPackage {
          pname = "miden-vm";
          version = "0.5.0";
          src = self.inputs.miden-vm;
          buildType = "release";
          buildFeatures = ["executable" "concurrent"];
          nativeBuildInputs = [self'.packages.rust];
          doCheck = false;
          cargoLock = {
            lockFile = "${self.inputs.miden-vm}/Cargo.lock";
          };
        };

        # Haskell Toolchain
        packages.ghc = pkgs.ghc;
        packages.haskell-language-server = pkgs.haskell-language-server;

        devenv.shells.default = {
          name = "jusion";

          # https://devenv.sh/reference/options/
          packages = [
            # Miden
            self'.packages.miden

            # Haskell
            self'.packages.ghc
            self'.packages.haskell-language-server
            pkgs.cabal-install
            pkgs.ghcid
            pkgs.zlib

            # Rust
            self'.packages.rust
            pkgs.rust-analyzer
            pkgs.cargo-watch
          ];

          # Extra languages
          languages = {
            javascript.enable = true;
          };
        };
      };
      flake = {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.

      };
    };
}
