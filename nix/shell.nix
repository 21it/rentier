let nixpkgs = import ./nixpkgs.nix;
in
{
  pkgs ? import nixpkgs {
    overlays = import ./overlay.nix {
      inherit vimBackground vimColorScheme;
    };
  },
  vimBackground ? "light",
  vimColorScheme ? "PaperColor",
  firebaseApiKey,
  firebaseProjectId,
  firebaseMsgSenderId
}:
with pkgs;

stdenv.mkDerivation {
  name = "rentier-env";
  buildInputs = [
    /* IDE */
    haskell-ide
    /* Apps */
    postgresql
    /* Utils */
    git
    haskellPackages.yesod-bin
    nix-prefetch-scripts
    cabal2nix
    cacert
  ];

  TERM="xterm-256color";
  LANG="C.UTF-8";
  LC_ALL="C.UTF-8";
  GIT_SSL_CAINFO="${cacert}/etc/ssl/certs/ca-bundle.crt";
  NIX_SSL_CERT_FILE="${cacert}/etc/ssl/certs/ca-bundle.crt";
  NIX_PATH="/nix/var/nix/profiles/per-user/root/channels";
  FIREBASE_API_KEY=firebaseApiKey;
  FIREBASE_PROJECT_ID=firebaseProjectId;
  FIREBASE_MSG_SENDER_ID=firebaseMsgSenderId;
  shellHook = ''
    source ./nix/export-test-envs.sh
    sh ./nix/spawn-test-deps.sh

    export HOOGLEDB=/root/.hoogle
    if [ "$(ls -A $HOOGLEDB)" ]; then
      echo "hoogle database already exists..."
    else
      echo "building hoogle database..."
      stack --stack-yaml=/app/stack.yaml exec hoogle generate
    fi
  '';
}
