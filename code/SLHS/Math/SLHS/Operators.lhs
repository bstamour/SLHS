

module Math.SLHS.Operators
       ( add
       , subtract
       , multiply
       , divide
       , comultiply
       , codivide
       ) where


import Prelude hiding (subtract)
import Math.SLHS.Core
import Math.SLHS.Opinions


-----------------------------------------------------------------------------------------
-- Combinators.
-----------------------------------------------------------------------------------------


add        = wrap2 add'
subtract   = wrap2 subtract'
multiply   = wrap2 multiply'
divide     = wrap2 divide'
comultiply = wrap2 multiply'
codivide   = wrap2 divide'


-----------------------------------------------------------------------------------------
-- Implementations.
-----------------------------------------------------------------------------------------


wrap2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
wrap2 f ma mb = ma >>= \a -> mb >>= \b -> f a b


-- Binomial operators.


add' :: Binomial a -> Binomial a -> SLExpr (SLState a) (Binomial a)
add' = undefined


subtract' :: Binomial a -> Binomial a -> SLExpr (SLState a) (Binomial a)
subtract' = undefined


multiply' :: Binomial a -> Binomial a -> SLExpr (SLState a) (Binomial a)
multiply' = undefined


divide' :: Binomial a -> Binomial a -> SLExpr (SLState a) (Binomial a)
divide' = undefined


comultiply' :: Binomial a -> Binomial a -> SLExpr (SLState a) (Binomial a)
comultiply' = undefined


codivide' :: Binomial a -> Binomial a -> SLExpr (SLState a) (Binomial a)
codivide' = undefined
