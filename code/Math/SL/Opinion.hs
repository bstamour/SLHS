module Math.SL.Opinion where


import Math.SL.Core
import Math.SL.Frame
import Math.SL.State
import Control.Applicative
import qualified Data.Map as M


-- | A subjective opinion (hyper opinion.) Can be casted to other opinion
--   types through the constructors below.
data Opinion h a =
  Opinion
  { opHolder      :: Holder h           -- ^ The holder of the belief.
  , opFrame       :: Frame a            -- ^ The frame it's defined over.
  , opBelief      :: BeliefVector a     -- ^ Belief vector.
  , opUncertainty :: Rational           -- ^ uncertainty mass.
  , opBaseRate    :: BaseRateVector a   -- ^ base rate.
  }


-- | Create a standard hyper opinion.
opinion :: SL Int Int (Opinion Int Int)
opinion = pure . pure $ Opinion
          (Holder 0)
          (frame [])
          (BeliefVector (M.fromList []))
          0
          (BaseRateVector (M.fromList []))


-- | Create a focused binomial opinion about the frame {x, not x}.
binomialOpinion :: f -> SL h a (Opinion h a)
binomialOpinion = undefined


-- | Create a binomial opinion about a subset of the frame.
binomialOpinionSubset :: Frame f -> SL h a (Opinion h a)
binomialOpinionSubset = undefined


-- | Check if an opinion is a binomial opinion.
isBinomial :: (Opinion h f) -> Bool
isBinomial = undefined


-- | Check if an opinion is a multinomial opinion (NOT a hyper opinion.)
isMultinomial :: (Opinion h f) -> Bool
isMultinomial = undefined
