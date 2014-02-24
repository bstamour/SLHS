module Math.SL.Opinion where


import Math.SL.Frame
import Math.SL.Core
import Math.SL.State
import Math.SL.SLValue

import Data.Functor.Compose
import Control.Applicative

import qualified Data.Map as M


data BeliefVector a = BeliefVector deriving Show
data BaseRateVector a = BaseRateVector deriving Show


-- | A subjective opinion (hyper opinion.) Can be casted to other opinion
--   types through the constructors below.
data Opinion h a =
  Opinion
  { opHolder      :: Holder h           -- ^ The holder of the belief.
  , opFrame       :: Frame a            -- ^ The frame it's defined over.
  , opBelief      :: BeliefVector a     -- ^ Belief vector.
  , opUncertainty :: Rational           -- ^ uncertainty mass.
  , opBaseRate    :: BaseRateVector a   -- ^ base rate.
  } deriving (Show)


opinion :: Int -> Holder h -> SLState h a (SLValue (Opinion h a))
opinion n holder = do frm <- getFrame n
                      return $ Opinion
                        <$> pure holder
                        <*> frm
                        <*> pure BeliefVector
                        <*> pure 0
                        <*> pure BaseRateVector
