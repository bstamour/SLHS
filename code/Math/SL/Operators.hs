
module Math.SL.Operators
       ( (<++>)
       , (</\>)
       ) where


import Math.SL.Opinion
import Math.SL.Core
import Math.SL.State
import Math.SL.SLValue

import Control.Applicative
import Control.Monad


wrapOper :: (Opinion h a -> Opinion h a -> SLValue (Opinion h a))
            -> SL h a (Opinion h a)
            -> SL h a (Opinion h a)
            -> SL h a (Opinion h a)
wrapOper oper op1 op2 = run <$> op1 <*> op2
  where
    run o1 o2 = join $ oper <$> o1 <*> o2


(<++>) :: SL h a (Opinion h a) -> SL h a (Opinion h a) -> SL h a (Opinion h a)
(<++>) = wrapOper impl
  where
    impl _ _ = SLError "(<++>): Not yet implemented."


(</\>) :: SL h a (Opinion h a) -> SL h a (Opinion h a) -> SL h a (Opinion h a)
(</\>) = wrapOper impl
  where
    impl _ _ = SLError "(</\\>): Not yet implemented."
