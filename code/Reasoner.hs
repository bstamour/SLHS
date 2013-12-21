

{- | This module represents the core reasoner architecture: reasoners, frames,
     belief mass assignments, and their respective instance declarations.

     For the time being, reasoners are defined only over one frame of discernment
     at a time, meaning certain SL operators (in particular, deduction and
     abduction) cannot work. I plan to solve this representational problem in
     the new year, but for now I want to focus on type-safe single frame reasoners.
-}


module Reasoner where


import Data.Maybe
import Control.Applicative
import Control.Monad
import qualified Data.Map as M


-- | A simple wrapper around frame types. For best effects, a should be bounded.
newtype Frame a = Frame a


data MassIndex f = Subset [f]    -- ^ Just a subset of the entire frame.
                 | Theta         -- ^ The whole thing.
                 deriving Eq


instance (Ord f) => Ord (MassIndex f) where
  (Subset x) < (Subset y) = x < y
  (Subset _) < Theta      = True
  Theta      < (Subset _) = False
  Theta      < Theta      = False


{- | Maps subsets of a frame f (or all of f) to fractions.

     Motivation:

     We use MassIndex types as the keys instead of the values directly
     so that users can freely use frames defined over massive types, such
     as Int, without having to store a key such as [minBound :: Int, maxBound :: Int]
     in the structure, which would cause massive slowdowns.
-}
newtype MassMap f = MassMap (M.Map (MassIndex f) Rational)


-- | Associates belief holders (h) to mass maps.
newtype MassAssignment h f = MassAssignment (M.Map h (MassMap f))


{- | The core reasoner type. Type-safe over the belief holders as well as the frame
     of discernment. One cannot combine reasoners that are of different types (for now.)

     To run a reasoner, you must supply it with a MassAssignment that is then threaded
     through the equations, which are built up using applicative combinators. All
     SL code must run within a reasoner to ensure type safety.
-}
newtype Reasoner h f a = Reasoner {
  unR :: MassAssignment h f -> a
}


instance Functor (Reasoner h f) where
  fmap f rx = Reasoner $ \mass -> f (unR rx mass)


instance Applicative (Reasoner h f) where
  pure x    = Reasoner $ \_ -> x

  rf <*> rx = Reasoner $ \mass -> let f = unR rf mass
                                      x = unR rx mass
                                  in f x


-- | Access the belief mass that we are towing around.
getMass :: (Ord h, Ord f) => h -> Reasoner h f (MassMap f)
getMass holder = Reasoner $ \ (MassAssignment m) ->
  fromMaybe emptyMass (M.lookup holder m)


-- | In the absense of any mass, we assume total uncertainty.
emptyMass :: Ord f => MassMap f
emptyMass = MassMap $ M.fromList [(Theta, 1)]


{- SL-related code.

   Below we define hyper opinions first, as they're the most general form
   of subjective opinion. Afterward we define multinomial opinions and methods
   to (possibly) generate a multinomial from a hyper. Finally the last, and
   most interesting class, is the binomial opinion.
-}


-- | The most general form of opinion in Subjective Logic.
data HyperOpinion f =
  HyperOpinion
  { hyperBelief      :: M.Map [f] Rational
  , hyperUncertainty :: Rational
  , hyperBaseRate    :: M.Map f Rational
  }


-- | Construct a hyper opinion from the belief mass. This must be constrained
--   to within the reasoner because it requires direct access to the belief mass.
opinion :: (Ord h, Ord f) => h -> Reasoner h f (HyperOpinion f)
opinion holder = go <$> getMass holder
  where
    go (MassMap mass) = let u = fromMaybe 0 (M.lookup Theta mass)
                            b = M.mapKeys (\(Subset s) -> s) $ M.delete Theta mass
                            a = M.empty -- TODO
                        in HyperOpinion b u a


-- | Not as general as a hyper opinion.
data MultinomialOpinion f =
  MultinomialOpinion
  { multiBelief      :: M.Map f Rational
  , multiUncertainty :: Rational
  , multiBaseRate    :: M.Map f Rational
  }


isMultinomial :: HyperOpinion f -> Bool
isMultinomial = M.foldrWithKey (\k _ x -> x && length k == 1) True . hyperBelief


-- | If the given hyper opinion is actually multinomial, then return it,
--   else return Nothing.
multinomial :: Ord f => HyperOpinion f -> Maybe (MultinomialOpinion f)
multinomial hyper = do guard (isMultinomial hyper)
                       let b = M.mapKeys (\k -> head k) $ hyperBelief hyper
                       let u = hyperUncertainty hyper
                       let a = hyperBaseRate hyper
                       return $ MultinomialOpinion b u a


-- | A binomial opinion is either a binary frame, or a binary partitioning
--   of a multinomial frame.
data BinomialOpinion f =
  BinomialOpinion
  { binBelief      :: Rational
  , binDisbelief   :: Rational
  , binUncertainty :: Rational
  , binBaseRate    :: Rational
  }


isBinomial :: MultinomialOpinion f -> Bool
isBinomial = (2 ==) . M.size . multiBelief


-- | Create a binomial opinion from a multinomial opinion if the multinomial
--   is actually a binomial opinion (frame has cardinality 2). Else, return
--   Nothing.
binomial :: Ord f => f -> MultinomialOpinion f -> Maybe (BinomialOpinion f)
binomial f multi = do guard (isBinomial multi)
                      b <- M.lookup f (multiBelief multi)
                      a <- M.lookup f (multiBaseRate multi)
                      let u = multiUncertainty multi
                      let d = 1 - b - u
                      return $ BinomialOpinion b d u a


-- | Coarsen a multinomial opinion down into a binomial opinion. This creates
--   a binomial opinion over a binary partitioning of the frame.
coarsen :: [f] -> MultinomialOpinion f -> BinomialOpinion f
coarsen subset multi = undefined


{- Subjective Logic operators -}


binomialSum :: BinomialOpinion f -> BinomialOpinion f -> BinomialOpinion f
binomialSum op1 op2 =
  let b = bx + by
      d = (ax * (dx - by) + ay * (dy - bx)) / (ax + ay)
      u = (ax * ux + ay * uy) / (ax + ay)
      a = ax + ay
  in BinomialOpinion b d u a
  where
    bx = binBelief op1
    by = binBelief op2
    dx = binDisbelief op1
    dy = binDisbelief op2
    ux = binUncertainty op1
    uy = binUncertainty op2
    ax = binBaseRate op1
    ay = binBaseRate op2


binomialDiff :: BinomialOpinion f -> BinomialOpinion f -> BinomialOpinion f
binomialDiff op1 op2 =
  let b = bx - by
      d = (ax * (dx + by) - ay * (1 + by - bx - uy)) / (ax - ay)
      u = (ax * ux - ay * uy) / (ax - ay)
      a = ax - ay
  in BinomialOpinion b d u a
  where
    bx = binBelief op1
    by = binBelief op2
    dx = binDisbelief op1
    dy = binDisbelief op2
    ux = binUncertainty op1
    uy = binUncertainty op2
    ax = binBaseRate op1
    ay = binBaseRate op2
