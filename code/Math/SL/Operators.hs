module Math.SL.Operators where


import Math.SL.Opinion
import Math.SL.Core
import Control.Applicative
import Control.Monad


-- Simply take an operator and do the plumbing for us. The reason why
-- the operators return SLValues instead of Opinions is so they can
-- catch runtime errors like bad frame of discernment, bad belief holders,
-- wrong opinion type, etc.
wrapOper :: (Opinion h1 f -> Opinion h2 g -> Either String (Opinion h3 h))
            -> SL (Opinion h1 f)
            -> SL (Opinion h2 g)
            -> SL (Opinion h3 h)
wrapOper oper op1 op2 = run <$> op1 <*> op2
  where
    run o1 o2 = join $ oper <$> o1 <*> o2


-- Some example operators. We can place restrictions on the types of frame,
-- but the actual values of the frame elements and the belief holders must
-- be checked at run time.


(<++>) :: SL (Opinion h f) -> SL (Opinion h f) -> SL (Opinion h f)
(<++>) = wrapOper impl
  where
    impl _ _ = Left "(<++>): Not yet implemented."


(</\>) :: SL (Opinion h f) -> SL (Opinion h f) -> SL (Opinion h f)
(</\>) = wrapOper impl
  where
    impl _ _ = Left "(</\\>): Not yet implemented."
