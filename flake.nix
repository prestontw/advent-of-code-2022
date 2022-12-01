{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/f1b9cc23aa8b1549dd7cb53dbe9fc950efc97646";
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
