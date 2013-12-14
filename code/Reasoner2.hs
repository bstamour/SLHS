{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}


{- This module contains the core reasoner code. A reasoner is a type

       Reasoner l h a t

   where
       l is the underlying logic that the reasoner uses in its computations.
       h is the type representing the belief holders.
       a is the type representing the atoms of the frame of discernment.
       t is the result of the computation.

   The reason why the holders and the atoms are baked into the type is to
   restrict what users can do with reasoners. Only reasoners of the same
   type can be combined, which means that users cannot stray out of one
   universe of discourse and into another.
-}


module Reasoner2 where


import Control.Applicative
import Data.Functor.Compose
import qualified Data.Map as M


-- | The core reasoner engine. Parameterized over the logic system l,
--   the set of possible belief holders h, and the atomic elements a that
--   comprise the frame of discernment. The resulting type is parameterized
--   over t: the result of the computation for the particular reasoner.
data Reasoner l h a t =
  Reasoner
  { unReasoner :: MassAssignment h a -> t
  }


-- | Maps belief holders to maps of subsets of the poweset of a to the set of
--   rational numbers.
data MassAssignment h a =
  MassAssignment
  { unMA :: M.Map h (M.Map [a] Rational)
  }


-- | Create a mass assignment from a list of pairs.
mkMass :: (Ord h, Ord a) => [(h, [([a], Rational)])] -> MassAssignment h a
mkMass masses = MassAssignment $ M.map M.fromList $ M.fromList masses


-- Functor and Applicative instances for the Reasoner type.

instance Functor (Reasoner l h a) where
  fmap f rx = Reasoner $ \m -> f (unReasoner rx m)


instance Applicative (Reasoner l h a) where
  pure x = Reasoner $ \_ -> x

  rf <*> rx = Reasoner $ \m -> let f = unReasoner rf m
                                   x = unReasoner rx m
                               in f x


-- | Each reasoner, whether single, composite, etc. has an associated
--   mass type. For single reasoners the mass type is simply the associated
--   MassAssignment. For composed reasoners, the mass type is the mass type
--   associated with the outermost reasoner.
type family MassType (r :: * -> *)

type instance MassType (Reasoner l h a) = MassAssignment h a
type instance MassType (Compose (Reasoner l h a) r2) = MassAssignment h a


-- | Access the mass within a reasoner.
getMass :: Reasoner l h a (MassAssignment h a)
getMass = Reasoner id


-- | A product reasoner is a reasoner defined over the cartesian product
--   of the two reasoners frames of discernment. This allows for computations
--   to take place in either reasoner and be defined in terms of both.
--
--   We use a type operator here to ease the creation of nested products:
--
--       type R = R1 :*: R2 :*: R3 :*: R4
--
--   is a valid reasoner type: it is the product of four smaller reasoners.
data (r1 :*: r2) t =
  ProductReasoner
  { unProductReasoner :: ProductMass (MassType r1) (MassType r2) -> t
  }


infixr :*:


-- | A cartesian product of mass assignments. Just stores the two masses
--   side-by-side for now.
data ProductMass m1 m2 = ProductMass { mass1 :: m1, mass2 :: m2 }


-- | A simple helper operator for multiplying mass assignments.
(<~>) :: m1 -> m2 -> ProductMass m1 m2
(<~>) = ProductMass


-- Functor and Applicative instances for the ProductReasoner.

instance Functor (r1 :*: r2) where
  fmap f prx = ProductReasoner $ \m -> f (unProductReasoner prx m)


instance Applicative (r1 :*: r2) where
  pure x = ProductReasoner $ \_ -> x

  prf <*> prx = ProductReasoner $ \m -> let f = unProductReasoner prf m
                                            x = unProductReasoner prx m
                                        in f x


type instance MassType (r1 :*: r2) = ProductMass (MassType r1) (MassType r2)


-- Access the masses of the ProductReasoner.

getProductMass :: (r1 :*: r2) (ProductMass (MassType r1) (MassType r2))
getProductMass = ProductReasoner id


getFirstMass :: (r1 :*: r2) (MassType r1)
getFirstMass = mass1 <$> getProductMass


getSecondMass :: (r1 :*: r2) (MassType r2)
getSecondMass = mass2 <$> getProductMass


-- | We want to be able to run all kinds of reasoners using the same interface.
class RunnableReasoner r t where
  type ResultType r t
  run :: r t -> MassType r -> ResultType r t


instance RunnableReasoner (Reasoner l h a) t where
  type ResultType (Reasoner l h a) t = t
  run = unReasoner


instance RunnableReasoner (Compose (Reasoner l h a) r2) t where
  type ResultType (Compose (Reasoner l h a) r2) t = r2 t
  run = unReasoner . getCompose


instance RunnableReasoner (r1 :*: r2) t where
  type ResultType ( r1 :*: r2) t = t
  run = unProductReasoner


-- | "Threading operator" to thread a mass assignment through a computation.
(<~~) :: RunnableReasoner r t => r t -> MassType r -> ResultType r t
(<~~) = run


-------------------------------------------------------------------------------------


{- This needs to be moved into a different module. -}


class Logic l where
  type Opinion l
  combine :: Reasoner l h a (Opinion l)
             -> Reasoner l h a (Opinion l)
             -> Reasoner l h a (Opinion l)


-------------------------------------------------------------------------------------


{- Here we flesh out the subjective logic operators. We make SL an instance of
   class Logic, with it's associated opinion (the hyper opinion) and for combination
   we choose cumulative fusion, though any combination operator should do the
   trick just as well.

   This needs to be moved to a different module.
-}


data SL = SL


data HyperOpinion = HyperOpinion


instance Logic SL where
  type Opinion SL = HyperOpinion
  combine = (<+>)


type SLHyper h a = Reasoner SL h a HyperOpinion


mkHyper :: h -> [a] -> SLHyper h a
mkHyper = undefined


(<+>) :: SLHyper h a -> SLHyper h a -> SLHyper h a
(<+>) = undefined


-------------------------------------------------------------------------------------


{- The following code is an example of how one can program using the SL Reasoner.
   There are two frames of discernment: colors, and sizes. We can define more
   complicated reasoners by multiplying the reasoners together. We can create
   functions to combine results from the separate reasoners into the larger one,
   and we can even do some programming with opinions.
-}


data Colors  = Red   | Blue | Yellow deriving (Eq, Ord)
data Sizes   = Small | Large         deriving (Eq, Ord)
data Holders = Bryan | Bob           deriving (Eq, Ord)


type SLColorReasoner     = Reasoner SL Holders Colors
type SLSizeReasoner      = Reasoner SL Holders Sizes
type SLColorSizeReasoner = SLColorReasoner :*: SLSizeReasoner


comb :: SLColorReasoner (Opinion SL)
        -> SLSizeReasoner (Opinion SL)
        -> SLColorSizeReasoner (Opinion SL)
comb op1 op2 = let x = run <$> pure op1 <*> getFirstMass
                   y = run <$> pure op2 <*> getSecondMass
               in const <$> x <*> y


type SLOperator h a = Reasoner SL h a (Opinion SL)
                      -> Reasoner SL h a (Opinion SL)
                      -> Reasoner SL h a (Opinion SL)


makeOpinion :: Reasoner SL h a (Opinion SL)
makeOpinion = mkHyper undefined undefined


(<&&>) :: SLOperator h a
op1 <&&> op2 = op2


(<||>) :: SLOperator h a
op1 <||> op2 = op1


expr :: Opinion SL
expr = (makeOpinion <||> makeOpinion) `comb` (makeOpinion <&&> makeOpinion)
       <~~
       (colorMass <~> sizeMass)
  where
    colorMass = mkMass []
    sizeMass  = mkMass []
