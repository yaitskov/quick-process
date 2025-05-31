{ haskell, lib, sources }:

let
  inherit (haskell.lib) doJailbreak dontCheck doHaddock overrideCabal;

  # 'fakeSha256' is helpful when adding new packages
  #
  # Set 'sha256 = fakeSha256', then replace the SHA with the one reported by
  # Nix when the build fails with a SHA mismatch error.
  inherit (lib) fakeSha256 nameValuePair listToAttrs;

in hfinal: hprev:

(listToAttrs (map (a:
  nameValuePair a.name
    (dontCheck (hfinal.callCabal2nix a.name a.source { }))) [
      { name = "th-utilities";  source = sources.th-utilities; }
      { name = "HList";  source = sources.HList; }
    ])) // {
      "th-lock" = hfinal.callHackageDirect
        { pkg = "th-lock";
          ver = "0.0.4";
          sha256 = "sha256-chFv77J0oWLzf4zAX4Awv7uhQEhiPegvPgrLWNaEuhs=";
        } {};

      "haddock-use-refs" = hfinal.callHackageDirect
        { pkg = "haddock-use-refs";
          ver = "1.0.1";
          sha256 = "sha256-fxrfMQ4CUthzNwYVjwV5kJmmPgimVpbnVhxnoYi1GrE=";
        } {};

      "trace-embrace" = hfinal.callHackageDirect
        { pkg = "trace-embrace";
          ver = "1.0.11";
          sha256 = "sha256-ggkNOAhOGScDPgberlOltMZKmHA4t15PGDtBKEeMYf0=";
        } {};
      # "HList" = hfinal.callHackageDirect
      #   { pkg = "HList";
      #     ver = "0.5.4.0";
      #     sha256 = "sha256-nzDQfZC22cEZ02661MKnRYUdKFOdq8lRg+ENfrA0PDk=";
      #   } {};
      "sbv" = overrideCabal
        (hfinal.callHackageDirect
          { pkg = "sbv";
            ver = "11.6";
            sha256 = "sha256-ND49xSYaMZCakzlsMMzCsDMAkxF8ZtJgeTNMan4iOaQ=";
          } {})

        (o: {testSystemDepends = o.testSystemDepends ++ [hfinal.z3];
             testDepends = (o.testDepends or []) ++ [hfinal.z3];
             extraLibraries = (o.extraLibraries or []) ++ [hfinal.z3];
             doCheck = false;
            })
      ;
    }
