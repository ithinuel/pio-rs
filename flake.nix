{
  description = "Flake configuration for pio-rs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
    git-hooks.url = "github:cachix/git-hooks.nix";
    git-hooks.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      utils,
      rust-overlay,
      git-hooks,
      ...
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ rust-overlay.overlays.default ];
        };

        stable = pkgs.rust-bin.stable."1.99.1";

        rust = stable.default.override {
          extensions = [
            "rust-src"
            "clippy"
            "rust-analyzer"
          ];
          targets = [ "thumbv6m-none-eabi" ];
        };

        # Hooks that pass through `nix flake check`. The nix sandbox has no
        # network access, so hooks needing to fetch anything (clippy) only run
        # in the dev shell / CI via `nix develop`.
        sandboxHooks = {
          nixfmt = {
            enable = true;
          };
          check-merge-conflicts.enable = true;
          check-added-large-files.enable = true;
          end-of-file-fixer = {
            enable = true;
            # vendored from raspberrypi/pico-examples, don't reformat
            excludes = [ "^tests/pico-examples/" ];
          };
          trim-trailing-whitespace = {
            enable = true;
            excludes = [ "^tests/pico-examples/" ];
          };
        };

        devHooks = sandboxHooks // {
          rustfmt = {
            enable = true;
            # keep the hooks on the pinned toolchain
            packageOverrides = {
              cargo = stable.cargo;
              rustfmt = stable.rustfmt;
            };
            settings = {
              all = true;
              check = true;
            };
          };
          clippy = {
            enable = true;
            # keep the hooks on the pinned toolchain so clippy matches rustc;
            # use the joined toolchain, the standalone clippy component's
            # driver resolves a sysroot without `std`.
            packageOverrides = {
              cargo = rust;
              clippy = rust;
            };
            settings = {
              allFeatures = true;
              denyWarnings = true;
              offline = false;
              extraArgs = "--workspace --all-targets";
            };
          };
        };

        hooks = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = devHooks;
        };
      in
      {
        devShells.default = pkgs.mkShell {
          inherit (hooks) shellHook;
          buildInputs = [ rust ] ++ hooks.enabledPackages;
        };

        # Sandbox-safe subset, exercised by `nix flake check`.
        checks.pre-commit = git-hooks.lib.${system}.run {
          src = ./.;
          hooks = sandboxHooks;
        };
      }
    );
}
