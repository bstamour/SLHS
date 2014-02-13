module SL where


import Control.Monad (ap, join)
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
--
-- TODO: We need a type to represent the frame of discernment. Frames can
-- be of different types, such as
--   1. Simple: just a set of atomic elements.
--   2. Products: we can have cartesian products of frames.
--   3. Conditional: these frames are seen in the deduction/abduction code. Might
--      not need to be treated special though. We will see...
--
-- We also need a belief mass assignment type. This needs to have fast lookup but
-- also should represent sparse assignments (lots of zeros.) Perhaps either a map
-- or a trie will work...

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

data Opinion = Opinion deriving Show


-- For now...
--
-- TODO: We can create lots of cool helper functions such as
--    1. opinion: create a hyper opinion
--    2. binomialOpinion x: create a binomial opinion about x
--    3. binomialOpinionSubset xs: create a coarsened binomial
--       opinion about the subset xs
--    4. binomialOpinionPred p: create a coarsened binomial opinion
--       about the subset x in xs where p(x) is true.
--    5. multinomialOpinion: somehow cast as a multinomial. Maybe don't
--       offer this one: make it a requirement for operators that they
--       have ``dirichlet'' mass instead of ``hyper-dirichlet''.
opinion :: SL Opinion
opinion = pure . pure $ Opinion


-- Simply take an operator and do the plumbing for us. The reason why
-- the operators return SLValues instead of Opinions is so they can
-- catch runtime errors like bad frame of discernment, bad belief holders,
-- wrong opinion type, etc.
wrapOper :: (Opinion -> Opinion -> SLValue Opinion)
            -> SL Opinion
            -> SL Opinion
            -> SL Opinion
wrapOper oper op1 op2 = run <$> op1 <*> op2
  where
    run :: SLValue Opinion -> SLValue Opinion -> SLValue Opinion
    run o1 o2 = join $ oper <$> o1 <*> o2


(<++>) :: SL Opinion -> SL Opinion -> SL Opinion
(<++>) = wrapOper impl
  where
    impl _ _ = Error "(<++>): Not yet implemented."


(</\>) :: SL Opinion -> SL Opinion -> SL Opinion
(</\>) = wrapOper impl
  where
    impl _ _ = Error "(</\\>): Not yet implemented."


-- A few test expressions.
e1 = opinion <++> opinion
e2 = opinion </\> (opinion <++> opinion)
e3 = (opinion </\> opinion) <++> (opinion </\> opinion)

runTest = flip runSL undefined