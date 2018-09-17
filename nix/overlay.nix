self: super:

{
  haskell = super.haskell // { packages = super.haskell.packages // { ghc822 = super.haskell.packages.ghc822.override {
     overrides =  hpkgs: _: {
       haskell-gi-overloading = hpkgs.haskell-gi-overloading_0_0;
     };
  };};};

}
