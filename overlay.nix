{ MathJax, ace-builds, reflex-dom-ace, reflex-dom-contrib, reflex-dom-pandoc
, reflex-platform, servant-subscriber, keycloak-config-cli-src, fira
, pkgs-unstable }:
let
  project = useWarp:
    reflex-platform.project ({ pkgs, ... }: {
      inherit useWarp;
      withHoogle = false;
      packages = {
        backend = ./backend;
        backend-api = ./backend-api;
        backend-extra = ./backend-extra;
        beam-crud = ./beam-crud;
        common = ./common;
        frontend = ./frontend;
        reflex-dom-hatex = ./reflex-dom-hatex;
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
          "reflex-dom-hatex"
          "servant-ac"
          "servant-ac-server"
          "servant-crud"
          "servant-crud-server"
          "servant-subscriber-reflex"
        ];
        ghcjs = [
          "common"
          "frontend"
          "reflex-dom-hatex"
          "servant-subscriber-reflex"
        ];
      };

      shellToolOverrides = ghc: super: {
        # haskell-language-server =
        #  (pkgs.callPackage ./nix/haskell-language-server.nix { }).override {
        #    supportedGhcVersions = [ "865" ];
        #  };
        brittany = ghc.brittany;
        inherit keycloak-config-cli;
        inherit (pkgs.xorg) libX11 libXcursor libXi libXrandr;
        inherit (pkgs-unstable.nodePackages) http-server;

        # This is because resolv is overridden and otherwise the original version is still used
        cabal-install = pkgs.haskellPackages.cabal-install;
      };

      overrides = with pkgs.haskell.lib;
        self: super: {
          backend = justStaticExecutables super.backend;
          backend-api = justStaticExecutables super.backend-api;

          reflex-dom-ace =
            doJailbreak (self.callCabal2nix "reflex-dom-ace" reflex-dom-ace { });
          reflex-dom-contrib = doJailbreak (
              self.callCabal2nix "reflex-dom-contrib" reflex-dom-contrib { });
          servant-subscriber =
            self.callCabal2nix "servant-subscriber" servant-subscriber { };
          reflex-dom-core = disableCabalFlag
            (disableCabalFlag super.reflex-dom-core "hydration-tests")
            "gc-tests";
          reflex-dom-pandoc =
            self.callCabal2nix "reflex-dom-pandoc" reflex-dom-pandoc { };

          unicode-data = self.callHackage "unicode-data" "0.1.0.1" { };
          streamly = self.callHackage "streamly" "0.8.1" { };
          curryer-rpc = self.callHackage "curryer-rpc" "0.2" { };

          TeX-my-math = dontCheck super.TeX-my-math;
          decimal-literals = unmarkBroken super.decimal-literals;
          haskeline = dontCheck (self.callHackage "haskeline" "0.8.0.0" { });
          http-link-header = doJailbreak super.http-link-header;
          modern-uri = self.callHackage "modern-uri" "0.3.4.0" { };
          oidc-client = dontCheck (self.callHackage "oidc-client" "0.6.0.0" { });
          project-m36 = dontCheck (self.callHackage "project-m36" "0.9.4" { });
          resolv = self.callHackage "resolv" "0.1.1.3" { };
          servant-aeson-specs = dontCheck (doJailbreak (unmarkBroken (super.servant-aeson-specs)));
          servant-docs = dontCheck super.servant-docs;
          servant-ekg = dontCheck (unmarkBroken super.servant-ekg);
          servant-quickcheck = dontCheck ( self.callHackage "servant-quickcheck" "0.0.10.0" { });
          true-name = doJailbreak (unmarkBroken super.true-name);
          universe-base = doJailbreak super.universe-base;
          vector-binary-instances = self.callHackage "vector-binary-instances" "0.2.5.1" { };
        };
    });

  pkgs = reflex-platform.nixpkgs;
  haskellLib = pkgs.haskell.lib;

  frontend-min = let
    frontend = haskellLib.justStaticExecutables (project true).ghcjs.frontend;
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
      mkdir -p $out/mathjax

      cp -r ${MathJax}/es5/* $out/mathjax
      cp ${./assets/mathjax-config.js} $out/mathjax/mathjax-config.js
      chmod +w -R $out/mathjax
      rm -r $out/mathjax/output/chtml*
      rm -r $out/mathjax/input/asciimath*
      rm -r $out/mathjax/input/mml*
      rm -r $out/mathjax/*chtml*
      rm -r $out/mathjax/*mml*

      mkdir $out/ace
      cp -r ${ace-builds}/src-min-noconflict/* $out/ace
      chmod +w -R $out/ace
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/ace*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/ext*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/keybinding*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/mode*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/theme*
      ${pkgs.zopfli}/bin/zopfli -i1000 $out/ace/worker*

      mkdir -p $out/fira/woff2
      cp -r ${fira}/woff2/* $out/fira/woff2
      cp ${./assets/fira.css} $out/fira/fira.css

      for file in $(find $out/{mathjax,fira} -type f)
      do
        echo "$file"
        ${pkgs.zopfli}/bin/zopfli -i1000 "$file"
      done
    '';
  };

  keycloak-nordtheme = ./keycloak-nordtheme;

  keycloak-config-cli = pkgs.callPackage ./nix/keycloak-config-cli {
    inherit keycloak-config-cli-src;
  };

  project-gtk = project false;

  frontend-gtk =
    let frontend = haskellLib.justStaticExecutables project-gtk.ghc.frontend;
    in pkgs.stdenv.mkDerivation rec {
      inherit (frontend) name version;

      nativeBuildInputs = [ pkgs.wrapGAppsHook ];

      buildInputs = [ pkgs.glib ];

      unpackPhase = "true";

      installPhase = ''
        mkdir -p $out/bin
        cp ${frontend}/bin/frontend $out/bin/frontend-gtk
      '';

      dontWrapGApps = true;

      postFixup = ''
        wrapProgram $out/bin/frontend-gtk \
          --set WEBKIT_DISABLE_COMPOSITING_MODE 1 \
          "''${gappsWrapperArgs[@]}"
      '';
    };

in {
  ferp-hs = project true // { inherit frontend-min vendor-lib frontend-gtk; };
  brittany = pkgs.haskellPackages.brittany;
  inherit keycloak-config-cli keycloak-nordtheme;
}
