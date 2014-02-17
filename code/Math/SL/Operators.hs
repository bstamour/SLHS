module Math.SL.Operators where


import Math.SL.Opinion
import Math.SL.Core

import Control.Applicative
import Control.Monad


wrapOper :: (Opinion h1 f -> Opinion h2 g -> SLValue (Opinion h3 h))
            -> SL (Opinion h1 f)
            -> SL (Opinion h2 g)
            -> SL (Opinion h3 h)
wrapOper oper op1 op2 = run <$> op1 <*> op2
  where
    run o1 o2 = join $ oper <$> o1 <*> o2


(<++>) :: SL (Opinion h f) -> SL (Opinion h f) -> SL (Opinion h f)
(<++>) = wrapOper impl
  where
    impl _ _ = SLError "(<++>): Not yet implemented."


(</\>) :: SL (Opinion h f) -> SL (Opinion h f) -> SL (Opinion h f)
(</\>) = wrapOper impl
  where
    impl _ _ = SLError "(</\\>): Not yet implemented."
