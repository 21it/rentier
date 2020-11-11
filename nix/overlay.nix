{
  hexOrganization,
  hexApiKey,
  robotSshKey,
  vimBackground ? "light",
  vimColorScheme ? "PaperColor"
}:
[
  (self: super:
    let
      callPackage = self.lib.callPackageWith self.haskellPackages;
      dontCheck = self.haskell.lib.dontCheck;
      doJailbreak = self.haskell.lib.doJailbreak;
    in
      {
        haskell-ide = import (
          fetchTarball "https://github.com/tim2CF/ultimate-haskell-ide/tarball/d455f6a3c7f7c363efa65da2ff003dbf3b4228d2"
        ) {inherit vimBackground vimColorScheme;};
        haskellPackages = super.haskell.packages.ghc865.extend(
          self': super': {
            universum = dontCheck super'.universum;
            proto3-suite = dontCheck (doJailbreak super'.proto3-suite);
            hspec-wai-json = callPackage ./overlay/hspec-wai-json.nix {};
            scotty = callPackage ./overlay/scotty.nix {};
            HaskellNet = callPackage ./overlay/haskell-net.nix {};
            hspec-wai = callPackage ./overlay/hspec-wai.nix {
              stdenv = self.stdenv;
            };
            concur-core = callPackage ./overlay/concur-core.nix {
              stdenv = self.stdenv;
              fetchgit = self.fetchgit;
            };
            replica = callPackage ./overlay/replica.nix {
              stdenv = self.stdenv;
              fetchgit = self.fetchgit;
            };
            concur-replica = callPackage ./overlay/concur-replica.nix {
              stdenv = self.stdenv;
              fetchgit = self.fetchgit;
            };
            persistent-migration = dontCheck (
              callPackage ./overlay/persistent-migration.nix {
                stdenv = self.stdenv;
                fetchgit = self.fetchgit;
            });
          }
        );
      })
]
