

module Math.SL.SLValue where


import Control.Applicative
import Control.Monad


-- | The result of an SL expression. Either it is a a value, or an error message.
data SLValue a = SLValue a | SLError String deriving (Show, Eq)


instance Functor SLValue where
  fmap f (SLValue x) = SLValue (f x)
  fmap _ (SLError e) = SLError e


instance Applicative SLValue where
  pure  = return
  (<*>) = ap


instance Monad SLValue where
  return x            = SLValue x
  (SLValue x)   >>= f = f x
  (SLError err) >>= _ = SLError err
