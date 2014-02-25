module Math.SL.Operators where


import Math.SL.Opinion
import Math.SL.Core
import Math.SL.Frame
import Math.SL.State
import Math.SL.SLValue

import Data.Functor.Compose
import Control.Applicative
import Control.Monad


type BinaryOp h a r = SLState h a (SLValue (Opinion h a))
                      -> SLState h a (SLValue (Opinion h a))
                      -> SLState h a (SLValue (Opinion h r))


wrapOper :: (Monad m, Applicative m, Applicative f)
            => (a -> b -> m c)
            -> f (m a) -> f (m b) -> f (m c)
wrapOper oper fma fmb = run <$> fma <*> fmb
  where
    run ma mb = join $ oper <$> ma <*> mb


-------------------------------------------------------------------------------------------
-- Single-frame operators.
-------------------------------------------------------------------------------------------


add :: Ord a => BinaryOp h a a
add = wrapOper impl
  where
    impl op1 op2
      | not (isBinomial op1) || not (isBinomial op2) =
        SLError "Only defined for binomial opinions."
      | otherwise =
        let b1 = binomialBelief op1
            b2 = binomialBelief op2
            d1 = binomialDisbelief op1
            d2 = binomialDisbelief op2
            u1 = uncertainty op1
            u2 = uncertainty op2
            a1 = binomialBaseRate op1
            a2 = binomialBaseRate op2

            b = undefined
            d = undefined
            u = undefined
            a = undefined
        in pure $ CoarsenedOpinion
           (holder op1)
           (opFrame op1)
           (frame []) -- TODO: Fixme.
           (frame []) -- TODO: Fixme.
           b
           d
           u
           a







subtract :: BinaryOp h a a
subtract = liftA2 impl
  where
    impl _ _ = SLError "subtract: Not yet implemented."


cFuse :: BinaryOp h a a
cFuse = liftA2 impl
  where
    impl _ _ = SLError "cFuse: Not yet implemented."


aFuse :: BinaryOp h a a
aFuse = liftA2 impl
  where
    impl _ _ = SLError "aFuse: Not yet implemented."


constrain :: BinaryOp h a a
constrain = liftA2 impl
  where
    impl _ _ = SLError "constrain: Not yet implemented."


-------------------------------------------------------------------------------------------
-- Operators defined over two frames.
-------------------------------------------------------------------------------------------


multiply :: BinaryOp h a (a, a)
multiply = liftA2 impl
  where
    impl _ _ = SLError "multiply: Not yet implemented."


comultiply :: BinaryOp h a (a, a)
comultiply = liftA2 impl
  where
    impl _ _ = SLError "comultiply: Not yet implemented."


divide :: BinaryOp h a (a, a)
divide = liftA2 impl
  where
    impl _ _ = SLError "divide: Not yet implemented."


codivide :: BinaryOp h a (a, a)
codivide = liftA2 impl
  where
    impl _ _ = SLError "codivide: Not yet implemented."


deduce :: BinaryOp h a (a, a)
deduce = liftA2 impl
  where
    impl _ _ = SLError "deduce: Not yet implemented."


abduce :: BinaryOp h a (a, a)
abduce = liftA2 impl
  where
    impl _ _ = SLError "abduce: Not yet implemented."
