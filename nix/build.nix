{ mkDerivation, base, hpack, singletons, stdenv, vinyl }:
mkDerivation {
  pname = "data-flow";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base singletons vinyl ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base singletons vinyl ];
  testHaskellDepends = [ base singletons vinyl ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/data-flow#readme";
  license = stdenv.lib.licenses.unlicense;
}
