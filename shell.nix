let unstable = import ./nix { };
in (import ./. { }).shells.ghc.overrideAttrs
(old: { inherit (unstable.pre-commit-check) shellHook; })
