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
      url = "path:/home/stefan/Projects/reflex-dom-pandoc";
      flake = false;
    };

    reflex-platform = {
      url = "path:/home/stefan/Projects/reflex-platform";
      flake = false;
    };

    servant-subscriber = {
      url = "path:/home/stefan/Projects/servant-subscriber";
      flake = false;
    };

    keycloak-config-cli-src = {
      url = "github:adorsys/keycloak-config-cli";
      flake = false;
    };

    mvn2nix.url = "github:fzakaria/mvn2nix";
  };

  outputs = inputs@{ self, MathJax, ace-builds, flake-utils, pre-commit-hooks
    , reflex-dom-ace, reflex-dom-contrib, reflex-dom-pandoc, reflex-platform
    , servant-subscriber, keycloak-config-cli-src, mvn2nix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        reflex-platform-derivation = import reflex-platform {
          inherit system;
          nixpkgsOverlays = [ mvn2nix.overlay ];
        };
        pkgs = import ./overlay.nix {
          inherit MathJax ace-builds reflex-dom-ace reflex-dom-contrib
            reflex-dom-pandoc servant-subscriber keycloak-config-cli-src;
          reflex-platform = reflex-platform-derivation;
        };
      in {
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
            };
          };
        };

        devShell = pkgs.ferp-hs.shells.ghc.overrideAttrs (old: {
          inherit (self.checks.${system}.pre-commit-check) shellHook;
        });

        packages = {
          inherit (pkgs.ferp-hs) frontend-min;
          inherit (pkgs.ferp-hs.ghc) backend backend-api;
          inherit (pkgs) keycloak-config-cli;
        };
      });
}
