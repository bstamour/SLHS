module Opinion where


import Core
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
opinion :: Ord f => SL (Opinion Int f)
opinion = pure . pure $ Opinion
          (Holder 0)
          (Frame [])
          (BeliefVector (M.fromList []))
          0
          (BaseRateVector (M.fromList []))


-- | Create a focused binomial opinion about the frame {x, not x}.
binomialOpinion :: f -> SL (Opinion h f)
binomialOpinion = undefined


-- | Create a binomial opinion about a subset of the frame.
binomialOpinionSubset :: Frame f -> SL (Opinion h f)
binomialOpinionSubset = undefined


-- These functions remain outside of the container classes as they should
-- be able to be called from anywhere. Keep things as simple as possible,
-- but no simpler :-)


-- | Check if an opinion is a binomial opinion.
isBinomial :: (Opinion h f) -> Bool
isBinomial = (2 ==) . length . unFrame . opFrame


-- | Check if an opinion is a multinomial opinion (NOT a hyper opinion.)
isMultinomial :: (Opinion h f) -> Bool
--isMultinomial = all ((1 ==) . length . fst) . M.toList . opBelief
isMultinomial = undefined
