{
  description = "A functional reactive design system";

  inputs = {
    MathJax = {
      url = "github:mathjax/MathJax";
      flake = false;
    };

    ace-builds = {
      url = "github:ajaxorg/ace-builds";
      flake = false;
    };

    fira = {
      url = "github:mozilla/Fira";
      flake = false;
    };

    flake-utils.url = "github:numtide/flake-utils";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";

    reflex-dom-ace = {
      url = "github:SlimTim10/reflex-dom-ace";
      flake = false;
    };

    reflex-dom-contrib = {
      url = "github:reflex-frp/reflex-dom-contrib";
      flake = false;
    };

    reflex-dom-pandoc = {
      url = "github:stefanboere/reflex-dom-pandoc/dynamic-pandoc";
      flake = false;
    };

    reflex-platform = {
      url = "github:reflex-frp/reflex-platform";
      flake = false;
    };

    servant-subscriber = {
      url = "github:stefanboere/servant-subscriber/simple-request";
      flake = false;
    };

    keycloak-config-cli-src = {
      url = "github:adorsys/keycloak-config-cli";
      flake = false;
    };

    mvn2nix.url = "github:fzakaria/mvn2nix";

    naersk.url = "github:nix-community/naersk";

    moz_overlay = {
      url = "github:mozilla/nixpkgs-mozilla";
      flake = false;
    };

    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  };

  outputs = inputs@{ self, MathJax, ace-builds, flake-utils, pre-commit-hooks
    , reflex-dom-ace, reflex-dom-contrib, reflex-dom-pandoc, reflex-platform
    , servant-subscriber, keycloak-config-cli-src, mvn2nix, fira, naersk
    , moz_overlay, nixpkgs }:
    {
      nixosModules = {
        ferp-hs = ./nix/modules/ferp-hs.nix;
        keycloak-config = ./nix/modules/keycloak-config.nix;
      };

      overlay = final: prev: {
        inherit (self.packages.${final.system})
          keycloak-config-cli keycloak-nordtheme;
        ferp-hs = {
          inherit (self.packages.${final.system})
            frontend-min backend backend-api;
        };
      };
    } // flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        rust_overlay = final: prev: {
          moz-rust = ((prev.rustChannelOf {
            channel = "nightly";
            sha256 = "B3T/bVER66CE2z7Ocvk7Gn6hXwDZnW+5n/3C0H+HSOk=";
          }).rust.override { targets = [ "wasm32-unknown-unknown" ]; });
          naersk-lib = naersk.lib."${system}".override {
            cargo = final.moz-rust;
            rustc = final.moz-rust;
          };
        };
        pkgs-unstable = import nixpkgs {
          inherit system;
          overlays = [ (import moz_overlay) naersk.overlay rust_overlay ];
        };
        reflex-platform-derivation = import reflex-platform {
          inherit system;
          nixpkgsOverlays = [ mvn2nix.overlay ];
        };
        pkgs = import ./overlay.nix {
          inherit MathJax ace-builds reflex-dom-ace reflex-dom-contrib
            reflex-dom-pandoc servant-subscriber keycloak-config-cli-src fira
            pkgs-unstable;
          reflex-platform = reflex-platform-derivation;
        };
      in rec {
        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            tools.brittany = pkgs.brittany;
            hooks = {
              cabal-fmt.enable = true;
              hlint.enable = true;
              brittany.enable = true;
              nixfmt.enable = true;
              prettier.enable = true;
              rustfmt.enable = true;
            };
          };
        };

        devShell = pkgs.ferp-hs.shells.ghc.overrideAttrs (old: {
          inherit (self.checks.${system}.pre-commit-check) shellHook;
          # Fixes crashes of webkitgtk for spinner icon
          WEBKIT_DISABLE_COMPOSITING_MODE = "1";
          RUST_BACKTRACE = 1;
          LD_LIBRARY_PATH = "${pkgs.vulkan-loader}/lib";
          nativeBuildInputs = old.nativeBuildInputs
            ++ [ pkgs-unstable.openssl pkgs-unstable.pkgconfig ];
        });

        packages = {
          inherit (pkgs.ferp-hs)
            frontend-min frontend-gtk vendor-lib truck-param-js;
          inherit (pkgs.ferp-hs.ghc) backend backend-api;
          inherit (pkgs) keycloak-config-cli keycloak-nordtheme;
        };

        apps = { inherit (packages) backend backend-api; };
      });
}
