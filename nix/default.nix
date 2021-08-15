{ sources ? import ./sources.nix }:
let
  overlay = self: pkgs: {
    inherit (import sources.niv { }) niv;
    pre-commit-hooks-nix = import sources."pre-commit-hooks.nix";
    inherit (import sources.gitignore { inherit (pkgs) lib; }) gitignoreSource;
    pre-commit-check = self.pre-commit-hooks-nix.run {
      hooks.cabal-fmt.enable = true;
      hooks.hlint.enable = true;
      hooks.brittany.enable = true;
      hooks.nixfmt.enable = true;
      hooks.prettier.enable = true;
      src = self.gitignoreSource ../.;
    };
    customNodePackages = pkgs.lib.dontRecurseIntoAttrs
      (self.callPackage ./node { nodejs = self.nodejs; });
  };
in import sources.nixpkgs {
  overlays = [ overlay ];
  config = { };
}
