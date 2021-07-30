{ mkDerivation, base, extra, lib, mtl, sdl2, sdl2-image, text }:
mkDerivation {
  pname = "exe";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base extra mtl sdl2 sdl2-image text ];
  executableHaskellDepends = [ base extra mtl sdl2 sdl2-image text ];
  license = lib.licenses.mit;
}
