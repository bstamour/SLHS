module Math.SL.Opinion where


import Math.SL.Frame
import Math.SL.Core
import Math.SL.State
import Math.SL.SLValue

import Data.Functor.Compose
import Control.Applicative

import qualified Data.Map as M


data BeliefVector a   = BeliefVector deriving Show
data MBeliefVector a  = MBeliefVector deriving Show
data BaseRateVector a = BaseRateVector deriving Show


data Opinion h a = HyperOpinion
                   (Holder h)         -- The belief holder.
                   (Frame a)          -- The frame of discernment.
                   (BeliefVector a)   -- Beliefs over the reduced powerset.
                   Rational           -- Uncertainty.
                   (BaseRateVector a) -- Base Rates
                 | MultinomialOpinion
                   (Holder h)         -- The belief holder.
                   (Frame a)          -- The frame of discernment.
                   (MBeliefVector a)  -- The belief vector over the frame.
                   Rational           -- Uncertainty.
                   (BaseRateVector a) -- The base rate vector.
                 | BinomialOpinion
                   (Holder h)         -- The belief holder.
                   (Frame a)          -- The frame of discernment.
                   a                  -- x
                   a                  -- ~x
                   Rational           -- belief
                   Rational           -- disbelief
                   Rational           -- uncertainty
                   Rational           -- base rate
                 | CoarsenedOpinion
                   (Holder h)         -- The belief holder.
                   (Frame a)          -- The frame of discernment.
                   (Frame a)          -- x
                   (Frame a)          -- ~x
                   Rational           -- belief
                   Rational           -- disbelief
                   Rational           -- uncertainty
                   Rational           -- base rate


-------------------------------------------------------------------------------------------
-- Constructors.
-------------------------------------------------------------------------------------------


-- Create a hyper opinion from the belief mass assignment.
opinion :: Int -> Holder h -> SLState h a (SLValue (Opinion h a))
opinion n holder = do frm <- getFrame n
                      return $ HyperOpinion
                        <$> pure holder
                        <*> frm
                        <*> pure BeliefVector
                        <*> pure 0
                        <*> pure BaseRateVector


-- Create a binomial opinion around x, where x is an element of the frame.
highlight :: Opinion h a -> a -> SLState h a (SLValue (Opinion h a))
highlight = undefined


-- Create a binomial opinion around x, where x is a sub-frame.
coarsen :: Opinion h a -> Frame a -> SLState h a (SLValue (Opinion h a))
coarsen = undefined


-------------------------------------------------------------------------------------------
-- Querying functions.
-------------------------------------------------------------------------------------------


isBinomial :: Opinion h a -> Bool
isBinomial (BinomialOpinion _ _ _ _ _ _ _ _) = True
isBinomial (CoarsenedOpinion _ _ _ _ _ _ _ _) = True
isBinomial _ = False


-------------------------------------------------------------------------------------------
-- Accessor functions.
-------------------------------------------------------------------------------------------


holder :: Opinion h a -> Holder h
holder = undefined


opFrame :: Opinion h a -> Frame a
opFrame = undefined


hyperBelief :: Opinion h a -> Frame a -> SLState h a (SLValue Rational)
hyperBelief = undefined


multinomialBelief :: Opinion h a -> a -> SLState h a (SLValue Rational)
multinomialBelief = undefined


binomialBelief :: Opinion h a -> SLState h a (SLValue Rational)
binomialBelief = undefined


binomialDisbelief :: Opinion h a -> SLState h a (SLValue Rational)
binomialDisbelief = undefined


binomialBaseRate :: Opinion h a -> SLState h a (SLValue Rational)
binomialBaseRate = undefined


baseRate :: Opinion h a -> a -> SLState h a (SLValue Rational)
baseRate = undefined


uncertainty :: Opinion h a -> SLState h a (SLValue Rational)
uncertainty (HyperOpinion _ _ _ u _)         = pure $ pure u
uncertainty (MultinomialOpinion _ _ _ u _)   = pure $ pure u
uncertainty (BinomialOpinion _ _ _ _ _ _ u _)  = pure $ pure u
uncertainty (CoarsenedOpinion _ _ _ _ _ _ u _) = pure $ pure u
