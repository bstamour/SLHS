module Math.SL.Operators where

import Math.SL.Core
import Math.SL.State
import Math.SL.Opinion


type SL a t = SLState a (SLValue t)


--------------------------------------------------------------------------------------------
-- Operators that will only work on coarsened frames of {x, ~x}
--------------------------------------------------------------------------------------------


add, subtract, multiply, divide, comultiply, codivide
   :: SL a (Opinion (Coarsened a))
      -> SL a (Opinion (Coarsened a))
      -> SL a (Opinion (Coarsened a))

add = undefined

subtract = undefined

multiply = undefined

comultiply = undefined

divide = undefined

codivide = undefined


--------------------------------------------------------------------------------------------
-- Operators that will only work on binomial opinions.
--------------------------------------------------------------------------------------------






--------------------------------------------------------------------------------------------
-- Operators that will only work on multinomial opinions.
--------------------------------------------------------------------------------------------








--------------------------------------------------------------------------------------------
-- Operators that will only work on hyper opinions.
--------------------------------------------------------------------------------------------
