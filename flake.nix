{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/df2bcbbd1c2aa144261cf1b0003c889c075dc693";
    utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, utils, ... }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShell = with pkgs; mkShell {
          buildInputs = [
            dotnet-sdk_6
          ];
        };
      });
}
