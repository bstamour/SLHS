{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}


module Foo where


import Control.Monad
import Control.Monad.Trans
import Control.Applicative

import qualified Data.Map as M
import qualified Data.Set as S


---------------------------------------------------------------------------------------------
-- Binomial opinions.


data BinomialOpinion a =
  BinOp { b_belief      :: Rational
        , b_disbelief   :: Rational
        , b_uncertainty :: Rational
        , b_baseRate    :: Rational
        , b_x           :: a
        , b_not_x       :: a
        } deriving Show


class ToBinomial x where
  toBinomial :: x a -> BinomialOpinion a


instance ToBinomial BinomialOpinion where
  toBinomial = id


---------------------------------------------------------------------------------------------
-- Multinomial opinions.


data MultinomialOpinion a =
  MultiOp { m_belief      :: M.Map a Rational
          , m_uncertainty :: Rational
          , m_baseRate    :: M.Map a Rational
          , m_frame       :: S.Set a
          } deriving Show


class ToMultinomial x where
  toMultinomial :: Ord a => x a -> MultinomialOpinion a


instance ToMultinomial BinomialOpinion where
  toMultinomial (BinOp b d u a x y) = MultiOp bel u br f
    where
      bel = (M.fromList [(x, b), (y, d)])
      br  = (M.fromList [(x, a), (y, 1 - a)])
      f   = S.fromList [x, y]


instance ToMultinomial MultinomialOpinion where
  toMultinomial = id


---------------------------------------------------------------------------------------------
-- Hyper opinions.


data HyperOpinion a =
  HypOp { h_belief      :: M.Map [a] Rational
        , h_uncertainty :: Rational
        , h_baseRate    :: M.Map a Rational
        , h_frame       :: S.Set a
        } deriving Show


class ToHyper x where
  toHyper :: Ord a => x a -> HyperOpinion a


instance ToHyper BinomialOpinion where
  toHyper = toHyper . toMultinomial


instance ToHyper MultinomialOpinion where
  toHyper (MultiOp b u a f) = HypOp b' u a f
    where
      b' = M.mapKeys pure b


instance ToHyper HyperOpinion where
  toHyper = id


---------------------------------------------------------------------------------------------
-- Binomial Operators.


bin_add' :: BinomialOpinion a -> BinomialOpinion a -> BinomialOpinion a
bin_add' = undefined


bin_subtract' :: BinomialOpinion a -> BinomialOpinion a -> BinomialOpinion a
bin_subtract' = undefined


bin_multiply' :: BinomialOpinion a -> BinomialOpinion a -> BinomialOpinion a
bin_multiply' = undefined


bin_comultiply' :: BinomialOpinion a -> BinomialOpinion a -> BinomialOpinion a
bin_comultiply' = undefined


bin_divide' :: BinomialOpinion a -> BinomialOpinion a -> BinomialOpinion a
bin_divide' = undefined


bin_codivide' :: BinomialOpinion a -> BinomialOpinion a -> BinomialOpinion a
bin_codivide' = undefined


---------------------------------------------------------------------------------------------
-- The operators wrapped in our monad stack.


bin_add :: ToBinomial op
           => SLMonad a (op a)
           -> SLMonad a (op a)
           -> SLMonad a (BinomialOpinion a)
bin_add op1 op2 = do op1' <- op1
                     op2' <- op2
                     return $ bin_add' (toBinomial op1') (toBinomial op2')


bin_subtract :: ToBinomial op
                => SLMonad a (op a)
                -> SLMonad a (op a)
                -> SLMonad a (BinomialOpinion a)
bin_subtract op1 op2 = do op1' <- op1
                          op2' <- op2
                          return $ bin_subtract' (toBinomial op1') (toBinomial op2')


bin_multiply :: ToBinomial op
                => SLMonad a (op a)
                -> SLMonad a (op a)
                -> SLMonad a (BinomialOpinion a)
bin_multiply op1 op2 = do op1' <- op1
                          op2' <- op2
                          return $ bin_multiply' (toBinomial op1') (toBinomial op2')


-- And the rest...


bin_divide :: ToBinomial op
              => SLMonad a (op a)
              -> SLMonad a (op a)
              -> SLMonad a (BinomialOpinion a)
bin_divide op1 op2 = err "Not yet implemented."


---------------------------------------------------------------------------------------------
-- The types to hold it all together.


data SLValue a = Val a | Err String deriving Show


instance Functor SLValue where
  fmap f x = pure f <*> x


instance Applicative SLValue where
  pure = return
  (<*>) = ap


instance Monad SLValue where
  return = Val
  (Err s) >>= _ = Err s
  (Val x) >>= f = f x


data SLStateDatum a =
  SLStateDatum { slsFrame    :: S.Set a
               , slsBaseRate :: M.Map a Rational
               }


newtype SLStateData a = SLStateData [SLStateDatum a]


newtype SLState a b = SLState { unSLState :: SLStateData a -> (b, SLStateData a) }


instance Functor (SLState a) where
  fmap f x = pure f <*> x


instance Applicative (SLState a) where
  pure = return
  (<*>) = ap


instance Monad (SLState a) where
  return x = SLState $ \st -> (x, st)

  sa >>= f = SLState $ \st -> let (a, st') = unSLState sa st
                                  sb       = f a
                              in unSLState sb st'


get :: SLState a (SLStateData a)
get = SLState $ \st -> (st, st)


put :: SLStateData a -> SLState a ()
put st = SLState $ \st' -> ((), st)


---------------------------------------------------------------------------------------------
-- Monad transformers.


newtype SLValueT m a = SLValueT { runSLValueT :: m (SLValue a) }


instance Monad m => Functor (SLValueT m) where
  fmap f x = pure f <*> x


instance Monad m => Applicative (SLValueT m) where
  pure = return
  (<*>) = ap


instance Monad m => Monad (SLValueT m) where
  return = SLValueT . return . Val

  x >>= f = SLValueT $ do val <- runSLValueT x
                          case val of
                            Err str -> return (Err str)
                            Val y   -> runSLValueT $ f y


instance MonadTrans SLValueT where
  lift = SLValueT . (liftM Val)


newtype SLStateT a m b = SLStateT { runSLStateT :: SLStateData a -> m (b, SLStateData a) }


instance Monad m => Monad (SLStateT a m) where
  return x = SLStateT $ \s -> return (x, s)

  (SLStateT x) >>= f = SLStateT $ \s -> do (v, s') <- x s
                                           runSLStateT (f v) s'


instance MonadTrans (SLStateT a) where
  lift mx = SLStateT $ \st -> mx >>= return  . (, st)


---------------------------------------------------------------------------------------------
-- helper functions.


type SLMonad a b = SLValueT (SLState a) b


err :: Monad m => String -> SLValueT m a
err = SLValueT . return . Err


run :: SLMonad a b -> SLStateData a -> SLValue b
run expr = fst . unSLState (runSLValueT expr)


---------------------------------------------------------------------------------------------
-- Belief owners.


data Owner a where
  NoOwner   :: Owner a
  Owner     :: a -> Owner a
  Consensus :: Owner a -> Owner a -> Owner a
  Discount  :: Owner a -> Owner a -> Owner a
  AFuse     :: Owner a -> Owner a -> Owner a
  AFuse     :: Owner a -> Owner a -> Owner a
