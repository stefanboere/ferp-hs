{ system ? builtins.currentSystem }:
let
  unstable = import ./nix { };
  sources = import ./nix/sources.nix { };
in (import sources.reflex-platform { inherit system; }).project
({ pkgs, ... }: {
  useWarp = true;
  withHoogle = false;
  packages = {
    backend = ./backend;
    backend-api = ./backend-api;
    beam-crud = ./beam-crud;
    common = ./common;
    frontend = ./frontend;
    servant-ac = ./servant-ac;
    servant-crud = ./servant-crud;
    servant-crud-server = ./servant-crud-server;
  };

  shells = {
    ghc = [
      "backend"
      "backend-api"
      "beam-crud"
      "common"
      "frontend"
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
      reflex-dom-pandoc =
        self.callCabal2nix "reflex-dom-pandoc" ../reflex-dom-pandoc { };

      servant-aeson-specs =
        dontCheck (doJailbreak (unmarkBroken (super.servant-aeson-specs)));
      servant-quickcheck =
        doJailbreak (unmarkBroken (super.servant-quickcheck));
      dhall = doJailbreak (super.dhall);
    };

})
