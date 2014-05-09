\documentclass[thesis.tex]{subfiles}

\begin{document}

\ignore{
\begin{code}
module Math.SLHS.Operators
       ( add
       , subtract
       , product
       , coproduct
       , quotient
       , coquotient
       ) where

import Prelude hiding (subtract, product)
import Control.Monad (liftM2, join)
import Control.Applicative
import Math.SLHS.Core
import Math.SLHS.Opinions
\end{code}
}

\section{Operators}

\subsection{Binomial Operators}

We begin this section by describing the operators defined over \emph{Binomial opinions}.
Many of these operators have equivalents in standard logic or set theory. The operators
are defined in layers: the innermost layer contains the actual code to compute the
resulting values, and the outermost layer \emph{lifts} the operators to work within
the monadic context. We begin by implementing the innermost layer, whose functions are
denoted by a trailing prime symbol:

\begin{code}
add' :: Binomial a -> Binomial a -> SLExpr a (Binomial a)
add' (Binomial bx dx ux ax _ _) (Binomial by dy uy ay _ _) =
  return $ Binomial b' d' u' a' undefined undefined
  where
    b' = bx + by
    d' = (ax * (dx - by) + ay * (dy - bx)) / (ax + ay)
    u' = (ax * ux + ay * uy) / (ax + ay)
    a' = ax + ay


subtract' :: Binomial a -> Binomial a -> SLExpr a (Binomial a)
subtract' (Binomial bx dx ux ax _ _) (Binomial by dy uy ay _ _) =
  return $ Binomial b' d' u' a' undefined undefined
  where
    b' = bx - by
    d' = (ax * (dx + by) - ay * (1 + by - bx - uy)) / (ax - ay)
    u' = (ax * ux - ay * uy) / (ax - ay)
    a' = ax - ay


product' :: Binomial a -> Binomial a -> SLExpr a (Binomial a)
product' (Binomial bx dx ux ax _ _) (Binomial by dy uy ay _ _) =
  return $ Binomial b' d' u' a' undefined undefined
  where
    b' = bx * by + ((1 - ax) * bx * uy + (1 - ay) * ux * by)
         / (1 - ax * ay)
    d' = dx + dy - dx * dy
    u' = ux * uy + ((1 - ay) * bx * uy + (1 - ax) * ux * by)
         / (1 - ax * ay)
    a' = ax * ay


coproduct' :: Binomial a -> Binomial a -> SLExpr a (Binomial a)
coproduct' (Binomial bx dx ux ax _ _) (Binomial by dy uy ay _ _) =
  return $ Binomial b' d' u' a' undefined undefined
  where
    b' = bx + by - bx * by
    d' = dx * dy + (ax * (1 - ay) * dx * uy + (1 - ax) * ay * ux * dy)
         / (ax + ay - ax * ay)
    u' = ux * uy + (ay * dx * uy + ax * ux * dy)
         / (ax + ay - ax * ay)
    a' = ax + ay - ax * ay


quotient' :: Binomial a -> Binomial a -> SLExpr a (Binomial a)
quotient' (Binomial bx dx ux ax _ _) (Binomial by dy uy ay _ _) =
  return $ Binomial b' d' u' a' undefined undefined
  where
    b' = ay * (bx + ax * ux) / ((ay - ax) * (by + ay *uy))
         - ax * (1 - dx) / ((ay - ax) * (1 - dy))
    d' = (dx - dy) / (1 - dy)
    u' = ay * (1 - dx) / ((ay - ax) * (1 - dy))
         - ay * (bx + ax * ux) / ((ay - ax) * (bx + ay * uy))
    a' = ax / ay


coquotient' :: Binomial a -> Binomial a -> SLExpr a (Binomial a)
coquotient' (Binomial bx dx ux ax _ _) (Binomial by dy uy ay _ _) =
  return $ Binomial b' d' u' a' undefined undefined
  where
    b' = (bx - by) / (1 - by)
    d' = ((1 - ay) * (dx + (1 - ax) * ux)
          / ((ax - ay) * (dy + (1 - ay) * uy)))
         - (1 - ax) * (1 - bx) / ((ax - ay) * (1 - by))
    u' = ((1 - ay) * (1 - bx) / ((ax - ay) * (1 - by)))
         - ((1 - ay) * (dx + (1 - ax) * ux)
            / ((ax - ay) * (dy + (1 - ay) * uy)))
    a' = (ax - ay) / (1 - ay)
\end{code}

We then lift the above functions into the monad like so:

\begin{code}
add :: (ToBinomial op1, ToBinomial op2)
       => SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
add opx opy = join $ add' <$>
              (fmap toBinomial opx) <*> (fmap toBinomial opy)


subtract :: (ToBinomial op1, ToBinomial op2)
            => SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
subtract opx opy = join $ subtract' <$>
                   (fmap toBinomial opx) <*> (fmap toBinomial opy)


product :: (ToBinomial op1, ToBinomial op2)
            => SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
product opx opy = join $ product' <$>
                  (fmap toBinomial opx) <*> (fmap toBinomial opy)


coproduct :: (ToBinomial op1, ToBinomial op2)
              => SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
coproduct opx opy = join $ coproduct' <$>
                    (fmap toBinomial opx) <*> (fmap toBinomial opy)


quotient :: (ToBinomial op1, ToBinomial op2)
          => SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
quotient opx opy = join $ quotient' <$>
                   (fmap toBinomial opx) <*> (fmap toBinomial opy)


coquotient :: (ToBinomial op1, ToBinomial op2)
            => SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
coquotient opx opy = join $ coquotient' <$>
                     (fmap toBinomial opx) <*> (fmap toBinomial opy)
\end{code}

These are the functions that are exposed to the user of the combinator library.




\subsection{Multinomial Operators}

\begin{code}
m_multiply' :: Multinomial a -> Multinomial a -> SLExpr (SLState a) (Multinomial a)
m_multiply' = undefined
\end{code}




\end{document}
