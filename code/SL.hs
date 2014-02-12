module SL where


import Control.Monad (ap)
import Control.Applicative


-- A simple wrapper type to store either a value or an error message.

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

  (SLValue x) >>= f = f x
  (Error s)   >>= _ = Error s


-- A custom state monad for SL computations.

data SLState = SLState -- TODO


data SLExpr a = SLExpr { unSLExpr :: SLState -> a }


instance Functor SLExpr where
  fmap f sx = SLExpr $ \st -> f (unSLExpr sx st)


instance Applicative SLExpr where
  pure  = return
  (<*>) = ap


instance Monad SLExpr where
  return x = SLExpr $ \_ -> x

  sx >>= f = SLExpr $ \st -> let sy = f (unSLExpr sx st)
                             in unSLExpr sy st


-- Type wrapper for easier-looking code. Maybe make this a newtype and
-- wrap it properly.

type SL a = SLExpr (SLValue a)


runSL :: SL a -> SLState -> SLValue a
runSL = unSLExpr


-- Opinions and their operators.
--
-- TODO: Write all code in terms of hyper opinions. Specialize when applicable.
--
-- Write the operators to work internally directly on Opinion types, that way
-- we can do tricks with short-circuit computation (thank you, SLValue).
--
-- We will offer make_opinion functions to construct opinions. They require
-- access to the state in order to compute them. A typical operator may look
-- like this
--
-- op1 <*> op2 = do op1' <- op1
--                  op2' <- op2
--                  return $ op_impl <$> op1' <*> op2'
--   where
--     op_impl :: Opinion -> Opinion -> Opinion

data Opinion = Opinion deriving Show


(<++>) :: SL Opinion -> SL Opinion -> SL Opinion
op1 <++> op2  = pure $ Error "(<++>): Not yet implemented."


(</\>) :: SL Opinion -> SL Opinion -> SL Opinion
op1 </\> op2  = pure $ Error "(</\\>): Not yet implemented."