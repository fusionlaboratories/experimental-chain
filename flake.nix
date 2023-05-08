{
  description = "Proof of Concept for Jusion";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    devenv.url = "github:cachix/devenv";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.devenv.flakeModule
      ];

      # systems = [ "x86_64-linux" "i686-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];
      systems = [ "aarch64-darwin" ];

      perSystem = { config, self', inputs', pkgs, system, ... }: {
        # Per-system attributes can be defined here. The self' and inputs'
        # module parameters provide easy access to attributes of the same
        # system.

        # Equivalent to  inputs'.nixpkgs.legacyPackages.hello;
        # packages.default = pkgs.hello;

        devenv.shells.default = {
          name = "jusion";

          # https://devenv.sh/reference/options/
          # packages = []
          languages = {
            javascript.enable = true;
            haskell.enable = true;
            rust.enable = true;
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
