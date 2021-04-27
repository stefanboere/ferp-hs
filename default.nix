{ system ? builtins.currentSystem }:
let
  unstable = import ./nix { };
  sources = import ./nix/sources.nix { };
  reflex-platform = import sources.reflex-platform { inherit system; };
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
      reflex-dom-mmark = ./reflex-dom-mmark;
      servant-ac = ./servant-ac;
      servant-crud = ./servant-crud;
      servant-crud-server = ./servant-crud-server;
    };

    shells = {
      ghc = [
        "backend"
        "backend-api"
        "backend-extra"
        "beam-crud"
        "common"
        "frontend"
        "reflex-dom-mmark"
        "servant-ac"
        "servant-crud"
        "servant-crud-server"
      ];
      ghcjs = [ "common" "frontend" ];
    };

    shellToolOverrides = ghc: super: {
      #    haskell-language-server = unstable.haskell-language-server;
      xdotool = pkgs.xdotool;
    };

    overrides = with pkgs.haskell.lib;
      self: super: {
        # Prevents ghcjs build being stuck, see reflex-platform#717
        mmorph = self.callHackage "mmorph" "1.1.3" { };
        clay = self.callHackage "clay" "0.13.3" { };
        reflex-codemirror =
          self.callCabal2nix "reflex-codemirror" sources.reflex-codemirror { };
        reflex-dom-contrib = doJailbreak
          (self.callCabal2nix "reflex-dom-contrib" sources.reflex-dom-contrib
            { });
        oidc-client = dontCheck (self.callHackage "oidc-client" "0.6.0.0" { });

        servant-aeson-specs =
          dontCheck (doJailbreak (unmarkBroken (super.servant-aeson-specs)));
        servant-quickcheck =
          doJailbreak (unmarkBroken (super.servant-quickcheck));
        dhall = doJailbreak (super.dhall);
        mmark = dontHaddock super.mmark;
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
        > $out/all.min.js
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/all.min.js

      ${frontend}/bin/frontend-css > $out/style.css
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/style.css
    '';
  };

in project // { inherit frontend-min; }
