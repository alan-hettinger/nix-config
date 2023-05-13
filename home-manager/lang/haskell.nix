{ pkgs, ... }: {

  home.packages = with pkgs.haskellPackages; [
    ghc
    # haskell-language-server
    ghc
    hoogle
    arrows
    async
    criterion
    cabal-install
    haskintex

  ];

}
