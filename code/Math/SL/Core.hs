module Math.SL.Core where


import Math.SL.Frame
import qualified Data.Map as M


data Holder h = Holder h deriving Show


data BeliefMassAssignment h a =
  BeliefMassAssignment
  { unBMA :: M.Map h (M.Map (Frame a) Rational)
  }


data BaseRate h a =
  BaseRate
  { unBR :: M.Map h (M.Map a Rational)
  }


data SLData h a =
  SLData
  { sldFrame    :: Frame a
  , sldBMA      :: BeliefMassAssignment h a
  , sldBaseRate :: BaseRate h a
  }
