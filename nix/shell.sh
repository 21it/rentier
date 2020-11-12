#!/bin/sh

export IP=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}')
export VIM_BACKGROUND="${VIM_BACKGROUND:-light}"
export VIM_COLOR_SCHEME="${VIM_COLOR_SCHEME:-PaperColor}"
echo "host is $IP"
echo "starting nixos container..."
# enable for GUI apps
# xhost + $IP
docker run -it --rm \
  -e DISPLAY=$IP:0 \
  -e NIXPKGS_ALLOW_BROKEN=1 \
  -p 3000:3000 \
  -v "$(pwd):/app" \
  -v "nix:/nix" \
  -v "nix-19.09-root:/root" \
  -w "/app" nixos/nix:2.3 sh -c "
  ./nix/bootstrap.sh &&
  nix-shell ./nix/shell.nix --pure \
   --argstr vimBackground $VIM_BACKGROUND \
   --argstr vimColorScheme $VIM_COLOR_SCHEME \
   --argstr firebaseApiKey $FIREBASE_API_KEY \
   --argstr firebaseProjectId $FIREBASE_PROJECT_ID \
   --argstr firebaseMsgSenderId $FIREBASE_MSG_SENDER_ID \
   --option sandbox false \
   -v --show-trace
  "
