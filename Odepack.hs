module Odepack where

import Control.Monad
import Data.Foldable (for_)
import Data.HList.CommonMain
import Data.IORef
import Data.Vector (Vector, (!))
import Foreign.C
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Numeric.AD
import Odepack.Raw
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM

deSolve' [pun| f y rwork iwork atol rtol times
          h0 hmax hmin maxord mxstep mxhnil
          mxordn mxords delt maxl kmp
          fjac g mf jroot lwp liwp psol
          maxstep dtout stepOp checkIState |] =
    wrapF f $ \f ->
    wrapG g $ \g ->
    wrapPSOL psol $ \psol ->
    wrapFJAC fjac $ \fjac ->
    rtol   $ \ rtol ->
    atol   $ \ atol ->
    with 1 $ \ itask ->
    with 1 $ \ istate ->
    with 1 $ \ iopt -> 
    with 0 $ \ t ->
    with 1 $ \ tout -> do

  jroot <- jroot

  let neq = VM.length y

  rwork <- rwork $ case mf of
    MethodAdams                 -> 20 + 16*neq
    MethodBDF JacFull           -> 22 + 9*neq + neq^2
    MethodBDF (JacBanded ml mu) -> 22 + 10*neq * (2*ml + mu)*neq
    MethodBDFS nnz _            -> 20 + 2*nnz + div nnz 2 + (11 + 5)*neq
    MethodAuto jacType -> let
      lrs = case jacType of
        JacFull -> 22 + 9*neq + neq^2
        JacBanded ml mu -> 22 + 10*neq + (2*ml+mu)*neq
      in max (20 + 16*neq) lrs
    MethodPK m miter -> let
      maxord = case m of
        Adams -> 12
        BDF -> 5
      lenls = case miter of
        Iter -> 0
        SPIOM -> neq*(maxl+3) + maxl^2
        SPIGMR -> neq*(maxl+3+min 1 (maxl-kmp)) + (maxl+3)*maxl + 1
        PGC -> 6*neq
        PCGS -> 6*neq
        UserPsol -> 3*neq
      in 20 + neq*(maxord+1) + 3*neq + lenls + lwp

  iwork <- iwork $ case mf of
    MethodAdams -> 20
    MethodBDF _ -> 20 + neq
    MethodBDFS _ _ -> 30
    MethodAuto _ -> 20 + neq
    MethodPK _ Iter -> 30
    MethodPK _ SPIOM -> 30 + maxl
    MethodPK _ _ -> 30 + maxl + liwp
      
  iter <- newIORef (0 :: Int)
  VM.write rwork 4 h0
  VM.write rwork 5 hmax
  VM.write rwork 6 hmin
  VM.write rwork 7 delt

  VM.write iwork 4 maxord
  VM.write iwork 5 mxstep
  VM.write iwork 6 mxhnil
  VM.write iwork 7 mxordn
  VM.write iwork 8 mxords

  case mf of
    MethodPK _ m | m == SPIOM || m == SPIGMR -> do
          VM.write iwork 7 (fromIntegral maxl)
          VM.write iwork 8 (fromIntegral kmp)
    _ -> return ()

  let next step = do
        i <- readIORef iter
        times t tout i dtout
        writeIORef iter (i+1)
        continue <- stepOp
        checkIState istate
        when (continue && i<maxstep) $ do
          step
          next step

  let fnLsodeLike = case mf of
          MethodAdams   -> dlsode2
          MethodBDF {}  -> dlsode2
          MethodBDFS {} -> dlsodes2
          MethodAuto {} 
            | g == nullFunPtr -> dlsoda2
            | otherwise -> dlsodar2 g jroot
          MethodPK {}
            | g == nullFunPtr -> dlsodpk2 psol
            | otherwise -> dlsodkr2 g jroot psol
  fnLsodeLike next fjac mf f y t tout rtol atol itask istate iopt rwork iwork

deSolveDef = let 
  h0 = 0
  hmax = 0
  hmin = 0
  delt = 0

  maxord = 0
  mxstep = 0
  mxhnil = 0

  mxords = 0
  mxordn = 0

  maxl = 5
  kmp = 5

  lwp = 0
  liwp = 0

  rwork = VM.new
  iwork = VM.new

  atol f = f =<< VS.thaw (VS.fromList [1e-5])
  rtol f = f =<< VS.thaw (VS.fromList [1e-5])

  maxstep = 1000

  fjac = Nothing :: Maybe FJAC_
  g = Nothing :: Maybe G_
  psol = Nothing :: Maybe PSOL_

  jroot = VM.new 0

  mf = MethodAdams

  dtout = 1e-3

  times t tout i dtout = do
    t <- peek t
    poke tout (t + dtout)

  stepOp = return True :: IO Bool

  checkIState n = do
    n <- peek n
    when (n <= -3) $ error ("ISTATE=" ++ show n)

 in [pun| stepOp rwork iwork atol rtol times maxstep
          dtout h0 hmax hmin delt maxord mxstep mxhnil
          maxl kmp lwp liwp psol
          mxords mxordn fjac g jroot mf checkIState |]

-- | likely an unsafe method for getting a "Data.Vector.Storable.Mutable" out of a 'Ptr'
ptrToVS n p = do
    fp <- newForeignPtr_ p
    return (VM.unsafeFromForeignPtr0 fp (fromIntegral n))


mkFFJAC :: (forall a. RealFloat a => a -> Vector a -> Vector a)
  -> Record [ Tagged "f" F_, Tagged "fjac" (Maybe FJAC_) ]
mkFFJAC fn = let f = mkF fn
                 fjac = Just (mkFJAC fn)
        in [pun| f fjac |]

mkF :: (forall a. RealFloat a => a -> Vector a -> Vector a) -> F_
mkF fn neq t y dydt = do
  n <- peek neq
  dydt <- ptrToVS n dydt
  y <- ptrToVS n y
  t <- peek t
  VM.copy dydt =<< VS.thaw . V.convert . fn t . V.convert =<< VS.freeze y

mkFJAC :: (forall a. RealFloat a => a -> Vector a -> Vector a)
  -> FJAC_
mkFJAC fn neq t y ml mu pd nrowpd = do
  n <- peek neq
  nrowpd <- peek nrowpd
  y <- VS.freeze =<< ptrToVS n y
  t <- peek t
  let jac = jacobian (fn (realToFrac t)) (V.convert y)

  pd <- ptrToVS (nrowpd*n) pd

  let fi = fromIntegral
  for_ [0 .. n-1] $ \j ->
    VM.copy (VM.slice (fi (nrowpd*j)) (fi (nrowpd*j + n-1)) pd)
        =<< VS.unsafeThaw (V.convert (V.map (V.! fi j) jac))

-- * example function

exampleMain = do
    y0 <- VS.thaw (VS.fromList [2,0])
    deSolve' $ let
        mf = MethodAuto JacFull

        -- van-der-Pol example from odepk_prb1.f
        ffjac =  mkFFJAC $ \t y -> V.fromList
              [y ! 1,
               3 * (1 - y!0 * y!0) * y!1 - y!0]

        y = y0
        stepOp = do
          print =<< VS.freeze y0
          return True
        dtout = 2.214773875
     in ffjac .<++. [pun| y stepOp mf dtout |] .<++. deSolveDef
