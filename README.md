# odepack-hs
A haskell binding to odepack (references http://www.netlib.org/odepack/
http://people.sc.fsu.edu/~jburkardt/f77_src/odepack/odepack.html) using f2c.
Cabal does not seem to support calling a fortran compiler (gfortran), and a
custom build that does so is rather fragile
(http://hackage.haskell.org/package/hlbfgsb is broken less than two years after
it was written).

Current usage looks like you are writing fortran in haskell (Odepack/Tst.hs).
But something higher-level may be forthcoming.
