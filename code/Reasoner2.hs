{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Reasoner2 where


import Control.Applicative
import Data.Functor.Compose


data Reasoner l a t =
  Reasoner
  { unReasoner :: MassAssignment a -> t
  }


data MassAssignment a = MassAssignment a


instance Functor (Reasoner l a) where
  fmap f rx = Reasoner $ \m -> f (unReasoner rx m)


instance Applicative (Reasoner l a) where
  pure x = Reasoner $ \_ -> x

  rf <*> rx = Reasoner $ \m -> let f = unReasoner rf m
                                   x = unReasoner rx m
                               in f x


type family MassType (r :: * -> *)

type instance MassType (Reasoner l a) = MassAssignment a
type instance MassType (Compose (Reasoner l a) r2) = MassAssignment a


getMass :: Reasoner l a (MassAssignment a)
getMass = Reasoner id


data ProductReasoner r1 r2 t =
  ProductReasoner
  { unProductReasoner :: ProductMass (MassType r1) (MassType r2) -> t
  }


data ProductMass m1 m2 = ProductMass { mass1 :: m1, mass2 :: m2 }


instance Functor (ProductReasoner r1 r2) where
  fmap f prx = ProductReasoner $ \m -> f (unProductReasoner prx m)


instance Applicative (ProductReasoner r1 r2) where
  pure x = ProductReasoner $ \_ -> x

  prf <*> prx = ProductReasoner $ \m -> let f = unProductReasoner prf m
                                            x = unProductReasoner prx m
                                        in f x


type instance MassType (ProductReasoner r1 r2) =
  ProductMass (MassType r1) (MassType r2)


getProductMass :: ProductReasoner r1 r2 (ProductMass (MassType r1) (MassType r2))
getProductMass = ProductReasoner id


getFirstMass :: ProductReasoner r1 r2 (MassType r1)
getFirstMass = mass1 <$> getProductMass


getSecondMass :: ProductReasoner r1 r2 (MassType r2)
getSecondMass = mass2 <$> getProductMass


class RunnableReasoner r t where
  type ResultType r t
  run :: r t -> MassType r -> ResultType r t


instance RunnableReasoner (Reasoner l a) t where
  type ResultType (Reasoner l a) t = t
  run = unReasoner


instance RunnableReasoner (Compose (Reasoner l a) r2) t where
  type ResultType (Compose (Reasoner l a) r2) t = r2 t
  run = unReasoner . getCompose


instance RunnableReasoner (ProductReasoner r1 r2) t where
  type ResultType (ProductReasoner r1 r2) t = t
  run = unProductReasoner


(<~~) :: RunnableReasoner r t => r t -> MassType r -> ResultType r t
(<~~) = run


(<~>) :: m1 -> m2 -> ProductMass m1 m2
(<~>) = ProductMass


-------------------------------------------------------------------------------------


data SL = SL
data Colors = Red | Blue | Yellow
data Sizes = Small | Large


type SLColorReasoner = Reasoner SL Colors
type SLSizeReasoner = Reasoner SL Sizes
type SLColorSizeReasoner t = ProductReasoner SLColorReasoner SLSizeReasoner t


data Opinion = Opinion


combine :: SLColorReasoner Opinion
           -> SLSizeReasoner Opinion
           -> SLColorSizeReasoner Opinion
combine op1 op2 = let x = run <$> pure op1 <*> getFirstMass
                      y = run <$> pure op2 <*> getSecondMass
                  in const <$> x <*> y


type SLOperator a = Reasoner SL a Opinion
                    -> Reasoner SL a Opinion
                    -> Reasoner SL a Opinion


makeOpinion :: Reasoner SL a Opinion
makeOpinion = pure Opinion


(<&&>) :: SLOperator a
op1 <&&> op2 = op2


(<||>) :: SLOperator a
op1 <||> op2 = op1


expr :: Opinion
expr = (makeOpinion <||> makeOpinion) `combine` (makeOpinion <&&> makeOpinion)
       <~~
       (MassAssignment Red <~> MassAssignment Small)