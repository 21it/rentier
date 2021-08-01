let nixpkgs-master = import (import ./nixpkgs-master.nix) {};
in
{
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
          fetchTarball {
            url="https://github.com/tim2CF/ultimate-haskell-ide/tarball/aa29e246495f15531f25159e1fa8e1d2b8f2534b";
            sha256="05j9s5npkbvc4zg0bl9q0hcqhygcv7w453swhhc25ddflhxlr4r1";
          }) {inherit vimBackground vimColorScheme;};
        haskellPackages = super.haskell.packages.ghc865.extend(
          self': super': {
            yesod-bin = nixpkgs-master.haskellPackages.yesod-bin;
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
