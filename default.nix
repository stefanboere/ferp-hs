{ system ? builtins.currentSystem }:
let unstable = import ./nix { };
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
  };

  overrides = self: super: {
    # Prevents ghcjs build being stuck, see reflex-platform#717
    mmorph = self.callHackage "mmorph" "1.1.3" { };
    clay = self.callHackage "clay" "0.13.3" { };
  };

})
