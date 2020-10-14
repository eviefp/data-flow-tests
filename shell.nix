let
  tooling = import ./default.nix;

in
  tooling.haskell.ghc884.shell
  # tooling.pkgs.mkShell {
  #   nativeBuildInputs = [
  #     tooling.haskell.ghc88.ghc
  #     tooling.haskell.ghc88.haskell-language-server
  #     tooling.haskell.ghc88.stylish-haskell
  #     tooling.haskell.ghc88.hlint
  #     tooling.haskell.ghc88.cabal-install
  #   ];
  #   NIX_PATH = "nixpkgs=${tooling.pkgs.path}";
  # }
