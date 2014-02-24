module Math.SL.State where


import Math.SL.Frame
import Math.SL.SLValue
import Math.SL.Core

import Data.Functor.Compose
import Control.Applicative
import Control.Monad


newtype SLState h a t =
  SLState
  { runState :: [SLData h a] -> ([SLData h a], t)
  }


instance Monad (SLState h a) where
  return x = SLState $ \st -> (st, x)

  sa >>= f = SLState $ \st -> let (st', a) = runState sa st
                                  sb       = f a
                              in runState sb st'


instance Applicative (SLState h a) where
  pure  = return
  (<*>) = ap


instance Functor (SLState h a) where
  fmap f sa = pure f <*> sa


-- Accessor functions.


getFrames :: SLState h a [Frame a]
getFrames = getAllField sldFrame


getBMAs :: SLState h a [BeliefMassAssignment h a]
getBMAs = getAllField sldBMA


getBaseRates :: SLState h a [BaseRate h a]
getBaseRates = getAllField sldBaseRate


getFrame :: Int -> SLState h a (SLValue (Frame a))
getFrame = getSingleField sldFrame


getBMA :: Int -> SLState h a (SLValue (BeliefMassAssignment h a))
getBMA = getSingleField sldBMA


getBaseRate :: Int -> SLState h a (SLValue (BaseRate h a))
getBaseRate = getSingleField sldBaseRate


get :: SLState h a [SLData h a]
get = SLState $ \st -> (st, st)


getAllField :: (SLData h a -> b) -> SLState h a [b]
getAllField field = map field <$> get


getSingleField :: (SLData h a -> b) -> Int -> SLState h a (SLValue b)
getSingleField field n = pick <$> getAllField field
  where
    pick frms | n >= length frms = SLError "out of bounds"
              | otherwise        = SLValue (frms !! n)


runSL :: SLState h a t -> [SLData h a] -> t
runSL st s = snd $ runState st s
