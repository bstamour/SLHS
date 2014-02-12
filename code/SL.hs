module SL where


import Control.Monad (ap)
import Control.Applicative


data SLValue a = Error String
               | SLValue a
               deriving (Eq, Show)


instance Functor SLValue where
  fmap f (Error s)   = Error s
  fmap f (SLValue x) = SLValue (f x)


instance Applicative SLValue where
  pure  = return
  (<*>) = ap


instance Monad SLValue where
  return = SLValue

  (Error s)   >>= _ = Error s
  (SLValue x) >>= f = f x
