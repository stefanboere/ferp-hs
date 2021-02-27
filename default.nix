{ system ? builtins.currentSystem }:
let
  unstable = import ./nix { };
  sources = import ./nix/sources.nix { };
in (import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  useWarp = true;
  withHoogle = false;
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = [ "common" "backend" "frontend" ];
    ghcjs = [ "common" "frontend" ];
  };

  shellToolOverrides = ghc: super: {
    haskell-language-server = unstable.haskell-language-server;
    xdotool = pkgs.xdotool;
  };

  overrides = with pkgs.haskell.lib;
    self: super: {
      # Prevents ghcjs build being stuck, see reflex-platform#717
      mmorph = self.callHackage "mmorph" "1.1.3" { };
      clay = self.callHackage "clay" "0.13.3" { };
      reflex-dom-contrib = doJailbreak
        (self.callCabal2nix "reflex-dom-contrib" sources.reflex-dom-contrib
          { });
      reflex-dom-pandoc =
        self.callCabal2nix "reflex-dom-pandoc" sources.reflex-dom-pandoc { };
    };

})
