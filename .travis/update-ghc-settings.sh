# Hint at GHC that a newer/modern version of g++ is available (installed via
# apt). This is a bit of a hack, but is required for llvm-general (>= 3.5.*).
#
if [ $(which gcc-4.8) ] && [ -e stack.yaml ]; then
  sed -i'' -e 's,/usr/bin/gcc.*",/usr/bin/gcc-4.8",' $(stack path --programs 2>/dev/null | tail -n 1)/ghc-${GHC}/lib/ghc-${GHC}/settings
fi
