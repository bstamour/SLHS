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


data SLState = SLState -- TODO


data SLExpr a = SLExpr { unSLExpr :: SLState -> a }


instance Functor SLExpr where
  fmap f sx = SLExpr $ \st -> f (unSLExpr sx st)


instance Applicative SLExpr where
  pure  = return
  (<*>) = ap


instance Monad SLExpr where
  return x = SLExpr $ \_ -> x

  sx >>= f = SLExpr $ \st -> let x  = unSLExpr sx st
                                 sy = f x
                             in unSLExpr sy st


type SL a = SLExpr (SLValue a)


runSL :: SL a -> SLState -> SLValue a
runSL = unSLExpr


data Opinion = Opinion deriving Show


(<++>) :: SL Opinion -> SL Opinion -> SL Opinion
op1 <++> op2  = pure $ Error "(<++>): Not yet implemented."


(</\>) :: SL Opinion -> SL Opinion -> SL Opinion
op1 </\> op2  = pure $ Error "(</\\>): Not yet implemented."