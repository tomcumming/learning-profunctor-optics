{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
  };
  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages."${system}";
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        packages = [
          (pkgs.haskell-language-server.override { supportedGhcVersions = [ "9123" ]; })
          pkgs.haskell.compiler.ghc9123
          pkgs.cabal-install
        ];
      };
      formatter."${system}" = pkgs.nixpkgs-fmt;
    };
}
