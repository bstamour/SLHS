module Math.SL.Operators where

import Math.SL.Core
import Math.SL.Opinion


type CoarsenedBinary a = SLValue (Opinion (Coarsened a))
                         -> SLValue (Opinion (Coarsened a))
                         -> SLValue (Opinion (Coarsened a))


--------------------------------------------------------------------------------------------
-- Operators that will only work on coarsened frames of {x, ~x}
--------------------------------------------------------------------------------------------


add :: CoarsenedBinary a
add = undefined

subtract :: CoarsenedBinary a
subtract = undefined

multiply :: CoarsenedBinary a
multiply = undefined

comultiply :: CoarsenedBinary a
comultiply = undefined

divide :: CoarsenedBinary a
divide = undefined

codivide :: CoarsenedBinary a
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
