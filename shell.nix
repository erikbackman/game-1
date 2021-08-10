pkgs:

let
  package = import ./src/default.nix pkgs;

  sdlPkgs = with pkgs;
    [ SDL
      SDL_gfx
      SDL_image ];

  haskellTooling = with pkgs;
    [ ghcid
      cabal-install
      haskellPackages.hindent
      haskellPackages.ormolu
      haskellPackages.hasktags
      haskellPackages.hoogle
      (haskell-language-server.override { supportedGhcVersions = [ "8104" ]; })
    ];
in
pkgs.mkShell {
  nativeBuildInputs = [ pkgs.pkg-config ];
  inputsFrom = [ package.env ];
  buildInputs = haskellTooling ++ sdlPkgs;
  shellHook = ''
    alias cb="cabal v2-build"
    alias cr="cabal v2-run"
  '';
}
