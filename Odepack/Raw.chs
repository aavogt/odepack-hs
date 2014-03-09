module Odepack.Raw where

import Control.Applicative
import Control.Monad
import Foreign.Storable
import Foreign.Ptr
import Foreign.C
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VM

import Foreign.Marshal

#include "odepack.h"

-- * Mode flag

-- | a slight generalization of the mode flag in ODEPACK, since this data
-- helps to select the correct subroutine
data MF
  = MethodAdams            -- ^ nonstiff (LSODE with MF=10)
  | MethodBDF JacType      -- ^ stiff (LSODE with MF=2x)
  | MethodBDFS Int JacType -- ^ sparse stiff (LSODES): Int is @NNZ@ (an estimated
                            -- maximum number of nonzero elements) in the jacobian
  | MethodAuto JacType     -- ^ automatically chooses stiff or nonstiff (LSODA)
  | MethodPK MultistepMeth Corrector -- ^ LSODPK
  deriving (Show, Eq)

data MultistepMeth = Adams | BDF
  deriving (Show, Enum, Eq)

-- | MITER
data Corrector
  = Iter
  | SPIOM
  | SPIGMR
  | PGC
  | PCGS
  | UserPsol
  deriving (Show, Eq)

-- | JT
data JacType
  = JacFull
  | JacBanded Int Int -- ^ @ML,MU@
  deriving (Show, Eq)

-- ** Work array dimensions
withMF :: FunPtr FJAC_
  -> MF
  -> (FunPtr FJAC_ -> Ptr CInt -> IO a)
      -- ^ Ptr CInt is the MF or JT argument
  -> IO a
withMF j MethodAdams f = with 10 (f j)
withMF j (MethodBDF m) f = with n (f j) where
  n = case m of
      JacFull
          | j /= nullFunPtr -> 21
          | otherwise       -> 22
      JacBanded _ _
          | j /= nullFunPtr -> 24
          | otherwise       -> 25
withMF j (MethodAuto m) f = with n (f j) where
  n = case m of
      JacFull
        | j /= nullFunPtr -> 1
        | otherwise       -> 2
      JacBanded _ _ 
        | j /= nullFunPtr -> 4
        | otherwise       -> 5
withMF j (MethodPK m c) f = with n (f j) where
  n = 10 + 10* fromIntegral (fromEnum m) + nc
  nc = case c of
    Iter -> 0
    SPIOM  -> 1
    SPIGMR -> 2
    PGC -> 3
    PCGS -> 4
    UserPsol -> 9
    

-- * Abbreviations
-- ** Vectors
type MVec = VM.IOVector Double
type MVecI = VM.IOVector CInt

-- *** Conversions
withMVec v f = VM.unsafeWith v (f . (castPtr :: Ptr Double -> Ptr CDouble))

withMVecI v f = VM.unsafeWith v f

-- ** Functions
type F_
  = Ptr CInt -- ^ @neq@
  -> Ptr CDouble -- ^ @t@
  -> Ptr CDouble -- ^ @&y[1]@
  -> Ptr CDouble -- ^ @&dydt[1]@ output
  -> IO ()

-- | lsodar solves for g(i) = 0, i = 1 .. ng
type G_ = Ptr CInt -- ^ @neq@
  -> Ptr CDouble -- ^ @t@
  -> Ptr CDouble -- ^ @&y[1]@
  -> Ptr CInt    -- ^ @ng@
  -> Ptr CDouble -- ^ @&g[1]@ output
  -> IO ()

type FJAC_
  =  Ptr CInt    -- ^ @neq@
  -> Ptr CDouble -- ^ @t@
  -> Ptr CDouble -- ^ @y@
  -> Ptr CInt    -- ^ @ml@
  -> Ptr CInt    -- ^ @mu@
  -> Ptr CDouble -- ^ @pd@ output
  -> Ptr CInt    -- ^ @nrowpd@
  -> IO ()

type PSOL_
  = Ptr CInt     -- ^ @neq@
  -> Ptr CDouble -- ^ @t@
  -> Ptr CDouble -- ^ @Y(*)@
  -> Ptr CDouble -- ^ @FTY(*)@
  -> Ptr CInt    -- ^ @WK(*)@
  -> Ptr CDouble -- ^ @HL0@
  -> Ptr CDouble -- ^ @WP(*)@
  -> Ptr CInt    -- ^ @IWP(*)@
  -> Ptr CDouble -- ^ @B(*)@
  -> Ptr CInt    -- ^ @LR@
  -> Ptr CInt    -- ^ @IER@
  -> IO ()

-- *** Conversions
foreign import ccall "wrapper" wrapF_ :: F_ -> IO (FunPtr F_)
foreign import ccall "wrapper" wrapFJAC_ :: FJAC_ -> IO (FunPtr FJAC_)
foreign import ccall "wrapper" wrapG_ :: G_ -> IO (FunPtr G_)
foreign import ccall "wrapper" wrapPSOL_ :: PSOL_ -> IO (FunPtr PSOL_)

wrapF f withPtr = do
  ptr <- wrapF_ f
  r <- withPtr ptr
  freeHaskellFunPtr ptr
  return r

wrapFJAC Nothing withPtr = withPtr nullFunPtr
wrapFJAC (Just f) withPtr = do
  ptr <- wrapFJAC_ f
  r <- withPtr ptr
  freeHaskellFunPtr ptr
  return r

wrapG Nothing withG = withG nullFunPtr
wrapG (Just g) withG = do
  ptr <- wrapG_ g
  r <- withG ptr
  freeHaskellFunPtr ptr
  return r

wrapPSOL Nothing withPSOL = withPSOL nullFunPtr
wrapPSOL (Just psol) withPSOL = do
  ptr <- wrapPSOL_ psol
  r <- withPSOL ptr
  freeHaskellFunPtr ptr
  return r

-- * c2hs stuff
-- ** level 2

-- $level2
-- parameters itol and MVec/MVecI dimensions are calculated from other parameters


-- | c2hs's marshaling doesn't let you pass in the length of an array, so
-- it's done here instead, which is used to define the functions ending in 2
wrapCommon lsodeFn1 next f y t tout rtol atol itask istate iopt rwork iwork
  = with (case (VM.length rtol, VM.length atol) of
              (1,1) -> 1
              (1,_) -> 2
              (_,1) -> 3
              (_,_ ) ->4 ) $ \itol ->
    with (fromIntegral (VM.length y))     $ \neq ->
    with (fromIntegral (VM.length rwork)) $ \lrw ->
    with (fromIntegral (VM.length iwork)) $ \liw ->
    next $ lsodeFn1 f neq y t tout itol rtol
      atol itask istate iopt rwork lrw iwork liw

dlsode2  next jac mf = wrapCommon dlsode1  $ next . withMF jac mf
dlsodes2 next jac mf = wrapCommon dlsodes1 $ next . withMF jac mf
dlsoda2  next jac mf = wrapCommon dlsoda1  $ next . withMF jac mf

dlsodar2 g jroot next jac mf = wrapCommon dlsodar1 $ \ dlsodar' ->
  withMF jac mf $ \ jac jt ->
  with (fromIntegral (VM.length jroot)) $ \ng ->
  next $ dlsodar' jac jt g ng jroot

dlsodpk2 psol next jac mf = wrapCommon dlsodpk1 $ \ dlsodpk' ->
  withMF jac mf $ \jac jt ->
  next $ dlsodpk' jac psol jt

dlsodkr2 g jroot psol next jac mf = wrapCommon dlsodkr1 $ \ dlsodkr' ->
  withMF jac mf $ \jac mf ->
  with (fromIntegral (VM.length jroot)) $ \ng ->
  next $ dlsodkr' jac psol mf g ng jroot
 
-- ** level 1

-- $level1
-- only difference from level 0 is that Ptrs are extracted from MVec and MVecI 


{#fun dlsode_ as dlsode1 {
    castFunPtr `FunPtr F_',    -- F
    id `Ptr CInt',             -- NEQ,
    withMVec* `MVec',          -- Y,
    castPtr `Ptr Double',      -- T,
    castPtr `Ptr Double',      -- TOUT,
    id `Ptr CInt',             -- ITOL
    withMVec* `MVec',          -- RTOL
    withMVec* `MVec',          -- ATOL
    castPtr `Ptr CInt',        -- ITASK,
    id `Ptr CInt',             -- ISTATE
    id `Ptr CInt',             -- IOPT
    withMVec* `MVec',          -- RWORK
    id `Ptr CInt',             -- LRW,
    withMVecI* `MVecI',        -- IWORK
    id `Ptr CInt',             -- LIW
    castFunPtr `FunPtr FJAC_', -- JAC,
    id `Ptr CInt'              -- MF
  }  -> `()' #}

{#fun dlsodes_ as dlsodes1 {
    castFunPtr `FunPtr F_',    -- F
    id `Ptr CInt',             -- NEQ,
    withMVec* `MVec',          -- Y,
    castPtr `Ptr Double',      -- T,
    castPtr `Ptr Double',      -- TOUT,
    id `Ptr CInt',             -- ITOL
    withMVec* `MVec',          -- RTOL
    withMVec* `MVec',          -- ATOL
    castPtr `Ptr CInt',        -- ITASK,
    id `Ptr CInt',             -- ISTATE
    id `Ptr CInt',             -- IOPT
    withMVec* `MVec',          -- RWORK
    id `Ptr CInt',             -- LRW,
    withMVecI* `MVecI',         -- IWORK
    id `Ptr CInt',             -- LIW
    castFunPtr `FunPtr FJAC_', -- JAC,
    id `Ptr CInt'              -- MF
  }  -> `()' #}

{#fun dlsoda_ as dlsoda1 {
    castFunPtr `FunPtr F_',    -- F
    id `Ptr CInt',             -- NEQ,
    withMVec* `MVec',          -- Y,
    castPtr `Ptr Double',      -- T,
    castPtr `Ptr Double',      -- TOUT,
    id `Ptr CInt',             -- ITOL
    withMVec* `MVec',          -- RTOL
    withMVec* `MVec',          -- ATOL
    castPtr `Ptr CInt',        -- ITASK,
    id `Ptr CInt',             -- ISTATE
    id `Ptr CInt',             -- IOPT
    withMVec* `MVec',          -- RWORK
    id `Ptr CInt',             -- LRW,
    withMVecI* `MVecI',        -- IWORK
    id `Ptr CInt',             -- LIW
    castFunPtr `FunPtr FJAC_', -- JAC,
    id `Ptr CInt'              -- JT
  }  -> `()' #}

{#fun dlsodar_ as dlsodar1 {
    castFunPtr `FunPtr F_',    -- F
    id `Ptr CInt',             -- NEQ,
    withMVec* `MVec',          -- Y,
    castPtr `Ptr Double',      -- T,
    castPtr `Ptr Double',      -- TOUT,
    id `Ptr CInt',             -- ITOL
    withMVec* `MVec',          -- RTOL
    withMVec* `MVec',          -- ATOL
    castPtr `Ptr CInt',        -- ITASK,
    id `Ptr CInt',             -- ISTATE
    id `Ptr CInt',             -- IOPT
    withMVec* `MVec',          -- RWORK
    id `Ptr CInt',             -- LRW,
    withMVecI* `MVecI',        -- IWORK
    id `Ptr CInt',             -- LIW
    castFunPtr `FunPtr FJAC_', -- JAC,
    id `Ptr CInt',             -- JT
    castFunPtr `FunPtr G_',    -- G
    id `Ptr CInt',             -- NG
    withMVecI* `MVecI'          -- JROOT(NG)
  }  -> `()' #}

{#fun dlsodpk_ as dlsodpk1 {
    castFunPtr `FunPtr F_',    -- F
    id `Ptr CInt',             -- NEQ,
    withMVec* `MVec',          -- Y,
    castPtr `Ptr Double',      -- T,
    castPtr `Ptr Double',      -- TOUT,
    id `Ptr CInt',             -- ITOL
    withMVec* `MVec',          -- RTOL
    withMVec* `MVec',          -- ATOL
    castPtr `Ptr CInt',        -- ITASK,
    id `Ptr CInt',             -- ISTATE
    id `Ptr CInt',             -- IOPT
    withMVec* `MVec',          -- RWORK
    id `Ptr CInt',             -- LRW,
    withMVecI* `MVecI',        -- IWORK
    id `Ptr CInt',             -- LIW
    castFunPtr `FunPtr FJAC_', -- JAC
    castFunPtr `FunPtr PSOL_', -- PSOL
    id `Ptr CInt'              -- MF
  }  -> `()' #}

{#fun dlsodkr_ as dlsodkr1 {
    castFunPtr `FunPtr F_',    -- F
    id `Ptr CInt',             -- NEQ,
    withMVec* `MVec',          -- Y,
    castPtr `Ptr Double',      -- T,
    castPtr `Ptr Double',      -- TOUT,
    id `Ptr CInt',             -- ITOL
    withMVec* `MVec',          -- RTOL
    withMVec* `MVec',          -- ATOL
    castPtr `Ptr CInt',        -- ITASK,
    id `Ptr CInt',             -- ISTATE
    id `Ptr CInt',             -- IOPT
    withMVec* `MVec',          -- RWORK
    id `Ptr CInt',             -- LRW,
    withMVecI* `MVecI',        -- IWORK
    id `Ptr CInt',             -- LIW
    castFunPtr `FunPtr FJAC_', -- JAC,
    castFunPtr `FunPtr PSOL_', -- PSOL
    id `Ptr CInt',             -- MF
    castFunPtr `FunPtr G_',    -- G
    id `Ptr CInt',             -- NG
    withMVecI* `MVecI'         -- JROOT
  }  -> `()' #}

{- $TODO

> DLSODI  (RES, ADDA, JAC, NEQ, Y, YDOTI, T, TOUT, ITOL, RTOL, ATOL, ITASK, ISTATE, IOPT, RWORK, LRW, IWORK, LIW, MF )
> DLSOIBT
> DLSODIS (RES, ADDA, JAC, NEQ, Y, YDOTI, T, TOUT, ITOL, RTOL, ATOL, ITASK, ISTATE, IOPT, RWORK, LRW, IWORK, LIW, MF )

-}


dintdy2 nyh rwork t k = do
  dky <- VM.new nyh
  ier <- dintdy1 t k rwork (fromIntegral nyh) dky
  dky' <- VS.freeze dky
  return (do
    unless (ier == 0) (Left ier)
    Right dky')

{#fun dintdy_ as dintdy1 {
  withCDouble* `Double', -- T
  with_* `CInt', -- K
  elt21 `Ptr Double', -- RWORK(21)
  with_* `CInt', -- NYH
  withMVec* `MVec', -- NYH
  alloca- `CInt' peek*   -- IFLAG
  } -> `()' #}

withCDouble x f = with x (f . (castPtr :: Ptr Double -> Ptr CDouble))
with_ x = with x

elt21 p = castPtr p `plusPtr` 20


-- * level 0 (c2hs generated)
