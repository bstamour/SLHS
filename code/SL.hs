--------------------------------------------------------------------------------------------
--
-- A fun module of subjective logic code. All operators are defined within a special
-- applicative functor so that they can have their state (mass assignments, base rates)
-- threaded through seamlessly to the end user.

--------------------------------------------------------------------------------------------


module SL
       ( SLValue(..)     -- Consider which ones to export. We don't want people making
       , SLState(..)     -- objects without going through the smart constructors...
       , SL
       , Opinion(..)
       , runSL
       , opinion
       , (<++>)
       , (</\>)
       ) where


import Control.Monad (ap, join)
import Control.Monad.State
import Control.Applicative
import qualified Data.Map as M


-- | The result of an SL expression. Either it is a a value, or an error message.
data SLValue a = SLValue a | SLError String deriving (Show, Eq)


-- Things to tow around. For each belief holder, we need to assign a belief mass
-- assignment from which to construct opinions out of. Every holder may have their
-- own mass assignments and base rates, so those create a kind of "environment"
-- for the computations to live in.


data SLState = SLState


-- Type wrapper for easier-looking code. Maybe make this a newtype and
-- wrap it properly.


type SL a = State SLState (Either String a)


runSL :: SL a -> SLState -> SLValue a
runSL expr st = case evalState expr st of
  Left err -> SLError err
  Right x  -> SLValue x


-- TODO: We may need different kinds of frames:
--  1. regular old frames
--  2. cartesian frames (think multiplication)
--  3. conditional frames (see deduction/abduction)


-- | A belief holder.
newtype Holder h = Holder Int deriving (Show, Eq, Ord)


-- | A frame of discernment containing elements of type a.
newtype Frame a = Frame { unFrame :: [a] } deriving (Show, Eq, Ord)


-- TODO: Make wrapper classes for the maps inside here. We don't want to
-- expose implementation details to the end user.


-- | A subjective opinion (hyper opinion.) Can be casted to other opinion
--   types through the constructors below.
data Opinion h a =
  Opinion
  { opHolder      :: Holder h           -- ^ The holder of the belief.
  , opFrame       :: Frame a            -- ^ The frame it's defined over.
  , opBelief      :: M.Map [a] Rational -- ^ Belief vector.
  , opUncertainty :: Rational           -- ^ uncertainty mass.
  , opBaseRate    :: M.Map a Rational   -- ^ base rate.
  } deriving Show


-- | Create a standard hyper opinion.
opinion :: Ord f => SL (Opinion h f)
opinion = pure . pure $ Opinion  (Holder 0) (Frame []) (M.fromList []) 0 (M.fromList [])


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
isMultinomial = all ((1 ==) . length . fst) . M.toList . opBelief


-- Simply take an operator and do the plumbing for us. The reason why
-- the operators return SLValues instead of Opinions is so they can
-- catch runtime errors like bad frame of discernment, bad belief holders,
-- wrong opinion type, etc.
wrapOper :: (Opinion h1 f -> Opinion h2 g -> Either String (Opinion h3 h))
            -> SL (Opinion h1 f)
            -> SL (Opinion h2 g)
            -> SL (Opinion h3 h)
wrapOper oper op1 op2 = run <$> op1 <*> op2
  where
    run o1 o2 = join $ oper <$> o1 <*> o2


-- Some example operators. We can place restrictions on the types of frame,
-- but the actual values of the frame elements and the belief holders must
-- be checked at run time.


(<++>) :: SL (Opinion h f) -> SL (Opinion h f) -> SL (Opinion h f)
(<++>) = wrapOper impl
  where
    impl _ _ = Left "(<++>): Not yet implemented."


(</\>) :: SL (Opinion h f) -> SL (Opinion h f) -> SL (Opinion h f)
(</\>) = wrapOper impl
  where
    impl _ _ = Left "(</\\>): Not yet implemented."
