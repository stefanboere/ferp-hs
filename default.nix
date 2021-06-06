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
      #    haskell-language-server = unstable.haskell-language-server;
      xdotool = pkgs.xdotool;
    };

    overrides = with pkgs.haskell.lib;
      self: super: {
        # Prevents ghcjs build being stuck, see reflex-platform#717
        mmorph = self.callHackage "mmorph" "1.1.3" { };
        clay = self.callHackage "clay" "0.13.3" { };
        reflex-dom-ace =
          self.callCabal2nix "reflex-dom-ace" sources.reflex-dom-ace { };
        reflex-dom-contrib = doJailbreak
          (self.callCabal2nix "reflex-dom-contrib" sources.reflex-dom-contrib
            { });
        oidc-client = dontCheck (self.callHackage "oidc-client" "0.6.0.0" { });
        servant-subscriber =
          self.callCabal2nix "servant-subscriber" sources.servant-subscriber
          { };

        reflex-dom-pandoc =
          self.callCabal2nix "reflex-dom-pandoc" sources.reflex-dom-pandoc { };
        servant-aeson-specs =
          dontCheck (doJailbreak (unmarkBroken (super.servant-aeson-specs)));
        servant-quickcheck =
          doJailbreak (unmarkBroken (super.servant-quickcheck));
        dhall = doJailbreak (super.dhall);
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

      cp ${./assets/favicon.ico} $out/favicon.ico

      cp -r ${vendor-lib} $out/vendor
    '';
  };

  vendor-lib = pkgs.stdenv.mkDerivation rec {
    name = "frontend-vendor";
    version = "0.1.0.0";
    buildCommand = ''
      mkdir -p $out

      # cp ${sources.MathJax}/es5/tex-chtml.js $out/tex-chtml.js

      # ${pkgs.closurecompiler}/bin/closure-compiler \
      #  $out/tex-chtml.js \
      #  > $out/tex-chtml.min.js
      # ${pkgs.zopfli}/bin/zopfli -i1000 $out/tex-chtml.min.js

      mkdir -p $out/ace
      cp -r ${sources.ace-builds}/src-min-noconflict/* $out/ace

      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/ace*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/ext*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/keybinding*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/mode*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/theme*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/worker*
    '';
  };

in project // { inherit frontend-min vendor-lib; }
