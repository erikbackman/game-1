pkgs:

let
  package = import ./src/default.nix pkgs;
  haskellTooling = let
  in with pkgs;
    [ ghcid
      cabal-install
      haskellPackages.hindent
      haskellPackages.haskell-language-server ];
in
pkgs.mkShell {
  nativeBuildInputs = [ pkgs.pkg-config ];
  inputsFrom = [ package.env ];
  buildInputs = haskellTooling;
  shellHook = ''
    alias cb="cabal v2-build"
  '';
}
