{ cabal, QuickCheck }:

cabal.mkDerivation (self: {
  pname = "church-list";
  version = "0.0.1";
  src = ./.;
  buildDepends = [ QuickCheck ];
  meta = {
    homepage = "https://github.com/jwiegley/church-list";
    description = "Lazy lists with O(1) concatenation that, unlike dlists, allow inspection";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
    maintainers = [ self.stdenv.lib.maintainers.jwiegley ];
  };
})
