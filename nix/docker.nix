let nixpkgs = import ./nixpkgs.nix;
in
{
  pkgs ? import nixpkgs {}
}:
let pkg = import ./default.nix {};
in
with pkgs;

dockerTools.buildImage {
  name = "tkachuk-labs/rentier";
  contents = [ pkg ];

  config = {
    Cmd = [ "${pkg}/bin/rentier" ];
    ExposedPorts = {
      "80/tcp" = {};
    };
  };
}
