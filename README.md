# odepack-hs
A haskell binding to most of odepack (references http://www.netlib.org/odepack/
http://people.sc.fsu.edu/~jburkardt/f77_src/odepack/odepack.html) using f2c.

Cabal does not seem to support calling a fortran compiler (gfortran), and a
custom build that does so is rather fragile
(http://hackage.haskell.org/package/hlbfgsb is broken less than two years after
it was written).

Optional/keyword arguments are supported: see exampleMain in Odepack.hs
