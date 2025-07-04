{ system ? builtins.currentSystem or "x86_64-linux"
, ghc ? "ghc9122"
}:

let
  nix = import ./nix;
  pkgs = nix.pkgSetForSystem system {
    config = {
      allowBroken = true;
      allowUnfree = true;
    };
  };
  inherit (pkgs) lib;
  hsPkgSetOverlay = pkgs.callPackage ./nix/haskell/overlay.nix {
    inherit (nix) sources;
  };

  importZ3 = drv:
    drv.overrideAttrs (oa: {
      propagatedBuildInputs = (oa.propagatedBuildInputs or []) ++ [pkgs.z3];
    });

  importGit = drv:
    drv.overrideAttrs (oa: {
      buildInputs = (oa.buildInputs or []) ++ [pkgs.git];
    });

  sources = [
    "^(trace-embrace.yaml|src|test).*$"
    "^(sandbox-effect|verify-call-specs).*"
    "^(hlist|multi-containers|refined|conduit-find).*"
    "^changelog[.]md$"
    "^.*\\.cabal$"
  ];

  base = hsPkgs.callCabal2nix "quick-process" (lib.sourceByRegex ./. sources) { };
  quick-process-overlay = _hf: _hp: { quick-process = importGit (importZ3 base); };
  baseHaskellPkgs = pkgs.haskell.packages.${ghc};
  hsOverlays = [ hsPkgSetOverlay quick-process-overlay ];
  hsPkgs = baseHaskellPkgs.override (old: {
    overrides =
      builtins.foldl' pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
      hsOverlays;
  });

  hls = pkgs.haskell.lib.overrideCabal hsPkgs.haskell-language-server
    (_: { enableSharedExecutables = true; });

  shell = hsPkgs.shellFor {
    packages = p: [ p.quick-process ];
    nativeBuildInputs = (with pkgs; [
      cabal-install
      ghcid
      hlint
      niv
      pandoc
      z3
      git
    ]) ++ [ hls hsPkgs.upload-doc-to-hackage ];
    shellHook = ''
      export PS1='$ '
      echo $(dirname $(dirname $(which ghc)))/share/doc > .haddock-ref
    '';
  };

  quick-process = hsPkgs.quick-process;
in {
  inherit hsPkgs;
  inherit ghc;
  inherit pkgs;
  inherit shell;
  inherit quick-process;
}
