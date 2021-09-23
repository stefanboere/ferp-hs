{ MathJax, ace-builds, reflex-dom-ace, reflex-dom-contrib, reflex-dom-pandoc
, reflex-platform, servant-subscriber, keycloak-config-cli-src }:
let
  project = reflex-platform.project ({ pkgs, ... }: {
    useWarp = true;
    withHoogle = false;
    packages = {
      backend = ./backend;
      backend-api = ./backend-api;
      backend-extra = ./backend-extra;
      beam-crud = ./beam-crud;
      common = ./common;
      frontend = ./frontend;
      servant-ac = ./servant-ac;
      servant-ac-server = ./servant-ac-server;
      servant-crud = ./servant-crud;
      servant-crud-server = ./servant-crud-server;
      servant-subscriber-reflex = ./servant-subscriber-reflex;
    };

    shells = {
      ghc = [
        "backend"
        "backend-api"
        "backend-extra"
        "beam-crud"
        "common"
        "frontend"
        "servant-ac"
        "servant-ac-server"
        "servant-crud"
        "servant-crud-server"
        "servant-subscriber-reflex"
      ];
      ghcjs = [ "common" "frontend" "servant-subscriber-reflex" ];
    };

    shellToolOverrides = ghc: super: {
      haskell-language-server =
        (pkgs.callPackage ./nix/haskell-language-server.nix { }).override {
          supportedGhcVersions = [ "865" ];
        };
      brittany = ghc.brittany;
      inherit keycloak-config-cli;
    };

    overrides = with pkgs.haskell.lib;
      self: super: {
        # Prevents ghcjs build being stuck, see reflex-platform#717
        mmorph = self.callHackage "mmorph" "1.1.3" { };
        clay = self.callHackage "clay" "0.13.3" { };
        reflex-dom-ace = self.callCabal2nix "reflex-dom-ace" reflex-dom-ace { };
        reflex-dom-contrib = doJailbreak
          (self.callCabal2nix "reflex-dom-contrib" reflex-dom-contrib { });
        oidc-client = dontCheck (self.callHackage "oidc-client" "0.6.0.0" { });
        servant-subscriber =
          self.callCabal2nix "servant-subscriber" servant-subscriber { };
        reflex-dom-core = disableCabalFlag
          (disableCabalFlag super.reflex-dom-core "hydration-tests") "gc-tests";

        reflex-dom-pandoc =
          self.callCabal2nix "reflex-dom-pandoc" reflex-dom-pandoc { };
        servant-aeson-specs =
          dontCheck (doJailbreak (unmarkBroken (super.servant-aeson-specs)));
        servant-docs = dontCheck super.servant-docs;
        servant-quickcheck =
          self.callHackage "servant-quickcheck" "0.0.7.4" { };
        dhall = doJailbreak (super.dhall);

        monoid-subclasses = self.callHackage "monoid-subclasses" "0.4.6.1" { };

        generic-aeson = doJailbreak (unmarkBroken super.generic-aeson);
        true-name = doJailbreak (unmarkBroken super.true-name);
        servant-server = dontCheck super.servant-server;

        backend = justStaticExecutables super.backend;
        backend-api = justStaticExecutables super.backend-api;
      };
  });

  pkgs = reflex-platform.nixpkgs;
  haskellLib = pkgs.haskell.lib;

  frontend-min = let
    frontend = haskellLib.justStaticExecutables project.ghcjs.frontend;
    pname = "frontend";
  in pkgs.stdenv.mkDerivation rec {
    inherit (frontend) name version;
    buildCommand = ''
      mkdir -p $out
      cp ${frontend}/bin/${pname}.jsexe/{rts,lib,out,runmain}.js $out
      ${pkgs.closurecompiler}/bin/closure-compiler \
        ${frontend}/bin/${pname}.jsexe/all.js \
        --compilation_level=ADVANCED_OPTIMIZATIONS \
        --jscomp_off=checkVars \
        --externs=${frontend}/bin/${pname}.jsexe/all.js.externs \
        --externs="${./frontend}/externs.js" \
        > $out/all.min.js
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/all.min.js

      ${frontend}/bin/frontend-css > $out/style.css
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/style.css

      cp ${./assets/favicon.ico} $out/favicon.ico

      cp -r ${vendor-lib} $out/vendor
    '';
  };

  vendor-lib = pkgs.stdenv.mkDerivation rec {
    name = "frontend-vendor";
    version = "0.1.0.0";
    buildCommand = ''
      mkdir -p $out

      cp ${MathJax}/es5/tex-chtml.js $out/tex-chtml.js

      ${pkgs.closurecompiler}/bin/closure-compiler \
        $out/tex-chtml.js \
        > $out/tex-chtml.min.js
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/tex-chtml.min.js

      mkdir -p $out/ace
      cp -r ${ace-builds}/src-min-noconflict/* $out/ace

      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/ace*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/ext*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/keybinding*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/mode*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/theme*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/worker*
    '';
  };

  keycloak-nordtheme = ./keycloak-nordtheme;

  keycloak-config-cli = pkgs.callPackage ./nix/keycloak-config-cli {
    inherit keycloak-config-cli-src;
  };

in {
  ferp-hs = project // { inherit frontend-min vendor-lib; };
  brittany = pkgs.haskellPackages.brittany;
  inherit keycloak-config-cli keycloak-nordtheme;
}
