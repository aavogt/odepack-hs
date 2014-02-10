{-# LANGUAGE ForeignFunctionInterface #-}
module Odepack.Tst where

import Odepack.Raw
import Foreign.Marshal
import Foreign.Storable
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import qualified Data.Vector as V
import Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Storable.Mutable as VS
import Control.Monad

type C'F = Ptr CInt -- ^ neq
    -> Ptr CDouble -- ^ @t@
    -> Ptr CDouble -- ^ @&y[1]@
    -> Ptr CDouble -- ^ @&dydt[1]@ output
    -> IO ()

type C'FJAC
    =  Ptr CInt    -- ^ neq
    -> Ptr CDouble -- ^ t
    -> Ptr CDouble -- ^ y
    -> Ptr CInt    -- ^ ml
    -> Ptr CInt    -- ^ mu
    -> Ptr CDouble -- ^ @pd@ output
    -> Ptr CInt    -- ^ nrowpd
    -> IO ()

foreign import ccall "wrapper" wrapF :: C'F -> IO (FunPtr C'F)
foreign import ccall "wrapper" wrapFJAC :: C'FJAC -> IO (FunPtr C'FJAC)

ptrToArr ni nj p = do
    p' <- newForeignPtr_ p
    a <- peek ni
    b <- peek nj
    return $ VM.unsafeFromForeignPtr0 p' (fromIntegral (a*b))

-- translation of the very first example in
-- http://www.netlib.org/odepack/opkddemos
-- a van-der pol oscillator
odepackPrb1 =
  allocaArray 25 $ \ y ->
  allocaArray 697 $ \ rwork ->
  allocaArray 45 $ \ iwork ->
  alloca $ \ neq ->
  alloca $ \ t ->
  alloca $ \ tout ->
  alloca $ \ itol ->
  alloca $ \ rtol ->
  alloca $ \ atol ->
  alloca $ \ itask ->
  alloca $ \ istate ->
  alloca $ \ iopt ->
  alloca $ \ lrw ->
  alloca $ \ liw ->
  alloca $ \ mf -> do
  poke itol 1
  poke rtol 0
  poke atol 1e-6
  poke lrw 697
  poke liw 45
  poke iopt 0
  poke neq 2

  f1 <- wrapF $ \n t y ydot -> do
        [y1,y2] <- peekArray 2 y
        pokeArray ydot [y2, 3*(1 - y1*y1)*y2 - y1]

  jac1 <- wrapFJAC $ \ neq t y ml mu pd nrowpd -> do
        pd <- ptrToArr nrowpd neq pd
        nrowpd <- fromIntegral `fmap` peek nrowpd
        [y1,y2] <- peekArray 2 y
        VM.write pd 0 0
        VM.write pd 1 1
        VM.write pd nrowpd (-6*y1*y2 - 1)
        VM.write pd (1 + nrowpd) (3*(1- y1*y1))

  forM [1,2] $ \meth -> forM [0 .. 3] $ \miter -> do
    let mfv = 10*meth + miter
    poke mf mfv
    poke t 0
    pokeArray y [2,0]
    poke tout 1.39283880203
    poke itask 1
    poke istate 1
    print $ "solution with mf = " ++ show mfv
    replicateM_ 4 $ do
      c'dlsode_ (castFunPtr f1)
              neq y t tout itol rtol atol itask istate 
              iopt rwork lrw iwork liw (castFunPtr jac1) mf
      ys <- peekArray 2 y
      t' <- peek t
      putStrLn $ unwords $ map show (t':ys)

      poke tout . (+ 2.214773875) =<< peek tout
    
  return ()
