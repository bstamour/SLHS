\documentclass[thesis.tex]{subfiles}

\begin{document}


\ignore{
\begin{code}
module Math.SLHS.Operators.Binomial where

import Control.Monad (liftM2, join)
import Control.Applicative
import Math.SLHS.Types
import Math.SLHS.Opinions
\end{code}
}


\subsection{Binomial Operators}

We begin our treatment of the Subjective Logic operators by looking at those operators
designed to work with the simplest of objects: binomial opinions. Binomial operators are
split into three primary sections: \emph{logical and set-theoretical operators} are those
which have analogs to binary logical (conjunction, disjunction) or set theoretical
(union, difference) operations, \emph{trust transitivity operators} are operators that
deal with modelling transitive trust networks between agents, and \emph{reasoning operators}
support deductive and abductive reasoning.


\subsubsection{Logical and Set-Theoretical Operators}


We will begin with the simplest of binomial operators: those of addition and subtraction.
Addition of two binomial opinions corresponds to the set theoretic union operator. Given
two binomial opinions representing disjoint subsets of the same frame, the sum of the
two opinions represents the union of their respective subsets.






\begin{code}
(+!) :: (ToBinomial op1, ToBinomial op2)
       => SLExpr h a (op1 h (Subframe a))
       -> SLExpr h a (op2 h (Subframe a))
       -> SLExpr h a (Binomial h (Subframe a))
opx +! opy = do opx' <- toBinomial <$> opx
                opy' <- toBinomial <$> opy
                pure $ add' opx' opy'
\end{code}


\begin{code}
add' :: Binomial h (Subframe a) -> Binomial h (Subframe a) -> Binomial h (Subframe a)
add' (Binomial bx dx ux ax _) (Binomial by dy uy ay _) =
  let b' = bx + by
      d' = (ax * (dx - by) + ay * (dy - bx)) / (ax + ay)
      u' = (ax * ux + ay * uy) / (ax + ay)
      a' = ax + ay
  in Binomial b' d' u' a' undefined
\end{code}




Binomial subtraction is the inverse operation of addition. It is equivalent to
the set difference operator.


\begin{code}
(-!) :: (ToBinomial op1, ToBinomial op2)
        => SLExpr h a (op1 h (Subframe a))
        -> SLExpr h a (op2 h (Subframe a))
        -> SLExpr h a (Binomial h (Subframe a))
opx -! opy = do opx' <- toBinomial <$> opx
                opy' <- toBinomial <$> opy
                pure $ subtract' opx' opy'
\end{code}


\begin{code}
subtract' (Binomial bx dx ux ax _) (Binomial by dy uy ay _) =
  let b' = bx - by
      d' = (ax * (dx + by) - ay * (1 + by - bx - uy)) / (ax - ay)
      u' = (ax * ux - ay * uy) / (ax - ay)
      a' = ax - ay
  in Binomial b' d' u' a' undefined
\end{code}


Multiplication of two binomial opinions is equivalent to the logical \emph{and}
operator.


\begin{code}
(*!) :: (ToBinomial op1, ToBinomial op2)
        => SLExpr h a (op1 h a)
        -> SLExpr h a (op2 h a)
        -> SLExpr h a (Binomial h (Subframe (a, a)))
opx *! opy = do opx' <- toBinomial <$> opx
                opy' <- toBinomial <$> opy
                pure $ times' opx' opy'


times' (Binomial bx dx ux ax _) (Binomial by dy uy ay _) =
  let b' = bx * by + ((1 - ax) * bx * uy + (1 - ay) * ux * by)
           / (1 - ax * ay)
      d' = dx + dy - dx * dy
      u' = ux * uy + ((1 - ay) * bx * uy + (1 - ax) * ux * by)
           / (1 - ax * ay)
      a' = ax * ay
  in Binomial b' d' u' a' undefined
\end{code}


... and co-multiplication is equivalent to the logical \emph{or} operator.


\begin{code}
(~*!) :: (ToBinomial op1, ToBinomial op2)
         => SLExpr h a (op1 h a)
         -> SLExpr h a (op2 h a)
         -> SLExpr h a (Binomial h (Subframe (a, a)))
opx ~*! opy = do opx' <- toBinomial <$> opx
                 opy' <- toBinomial <$> opy
                 pure $ cotimes' opx' opy'




cotimes' (Binomial bx dx ux ax _) (Binomial by dy uy ay _) =
  let b' = bx + by - bx * by
      d' = dx * dy + (ax * (1 - ay) * dx * uy + (1 - ax) * ay * ux * dy)
           / (ax + ay - ax * ay)
      u' = ux * uy + (ay * dx * uy + ax * ux * dy)
           / (ax + ay - ax * ay)
      a' = ax + ay - ax * ay
  in Binomial b' d' u' a' undefined
\end{code}


Division is the inverse of multiplication.


\begin{code}
(/!) :: (ToBinomial op1, ToBinomial op2)
        => SLExpr h a (op1 h (Subframe (a, a)))
        -> SLExpr h a (op2 h a)
        -> SLExpr h a (Binomial h a)
opx /! opy = do opx' <- toBinomial <$> opx
                opy' <- toBinomial <$> opy
                pure $ divide' opx' opy'



divide' (Binomial bx dx ux ax _) (Binomial by dy uy ay _) =
  let b' = ay * (bx + ax * ux) / ((ay - ax) * (by + ay *uy))
           - ax * (1 - dx) / ((ay - ax) * (1 - dy))
      d' = (dx - dy) / (1 - dy)
      u' = ay * (1 - dx) / ((ay - ax) * (1 - dy))
           - ay * (bx + ax * ux) / ((ay - ax) * (bx + ay * uy))
      a' = ax / ay
  in Binomial b' d' u' a' undefined
\end{code}


And similarily, co-division is the inverse of co-multiplication.


\begin{code}
(~/!) :: (ToBinomial op1, ToBinomial op2)
         => SLExpr h a (op1 h (Subframe (a, a)))
         -> SLExpr h a (op2 h a)
         -> SLExpr h a (Binomial h a)
opx ~/! opy = do opx' <- toBinomial <$> opx
                 opy' <- toBinomial <$> opy
                 pure $ codivide' opx' opy'



codivide' (Binomial bx dx ux ax _) (Binomial by dy uy ay _) =
  let b' = (bx - by) / (1 - by)
      d' = ((1 - ay) * (dx + (1 - ax) * ux)
            / ((ax - ay) * (dy + (1 - ay) * uy)))
           - (1 - ax) * (1 - bx) / ((ax - ay) * (1 - by))
      u' = ((1 - ay) * (1 - bx) / ((ax - ay) * (1 - by)))
           - ((1 - ay) * (dx + (1 - ax) * ux)
              / ((ax - ay) * (dy + (1 - ay) * uy)))
      a' = (ax - ay) / (1 - ay)
  in Binomial b' d' u' a' undefined
\end{code}








\subsubsection{Trust Transitivity Operators}

In this section we implement the subjective logic operators for trust transitivity. If
two agents A and B exist such that A has an opinion about B's recommendation of some
proposition x, then A can generate an opinion about x by \emph{discounting} B's
recommendation of x based on A's opinion of B.

Subjective Logic offers three methods of discounting: \emph{uncertainty favouring
discounting}, \emph{opposite belief favouring discounting}, and \emph{base rate
sensitive discounting}.

We begin by constructing a simple algebraic data type to represent each of the three
kinds of discounting.




\begin{code}
data Favouring = Uncertainty | Opposite | BaseRateSensitive
\end{code}




By doing so, we are able to expose a single discounting function to the user with the
following signature:

\begin{spec}
discount :: Favouring
  -> SLExpr a (Binomial a) -> SLExpr a (Binomial a) -> SLExpr a (Binomial a)
\end{spec}

Since the arrow operator for function signatures is right associative, one can consider
\emph{discount} to be a function mapping discount favourings to a binary function over
binomial opinions much like the operators discussed in the previous section.

\begin{code}
discount_u :: Binomial h h -> Binomial h a -> Binomial h a
discount_u (Binomial bb db ub ab _) (Binomial bx dx ux ax _) =
  Binomial b' d' u' a' undefined
  where
    b' = bb * bx
    d' = bb * dx
    u' = db + ub + bb * ux
    a' = ax
\end{code}

Next, opposite belief favouring discounting:

\begin{code}
discount_o :: Binomial h h -> Binomial h a -> Binomial h a
discount_o (Binomial bb db ub ab _) (Binomial bx dx ux ax _) =
  Binomial b' d' u' a' undefined
  where
    b' = bb * bx + db * dx
    d' = bb * dx + db * bx
    u' = ub + (bb + db) * ux
    a' = ax
\end{code}

And lastly, base rate sensitive discounting:

\begin{code}
discount_b :: Binomial h h -> Binomial h a -> Binomial h a
discount_b op1@(Binomial bb db ub ab _) op2@(Binomial bx dx ux ax _) =
  Binomial b' d' u' a' undefined
  where
    b' = expectation op1 * bx
    d' = expectation op1 * dx
    u' = 1 - expectation op1 * (bx + dx)
    a' = ax
\end{code}



\begin{code}
discount :: (ToBinomial op1, ToBinomial op2)
            => Favouring
            ->SLExpr h a (op1 h h)
            -> SLExpr h a (op2 h a)
            -> SLExpr h a (Binomial h a)
discount f opx opy = do
  opx' <- toBinomial <$> opx
  opy' <- toBinomial <$> opy
  pure $ case f of
    Uncertainty       -> discount_u opx' opy'
    Opposite          -> discount_o opx' opy'
    BaseRateSensitive -> discount_b opx' opy'
\end{code}



\subsubsection{Reasoning Operators}







\begin{code}
deduce :: (ToBinomial op1, ToBinomial op2, ToBinomial op3)
          => SLExpr h a (op1 h a)
          -> SLExpr h a (op2 h a)
          -> SLExpr h a (op3 h a)
          -> SLExpr h a (Binomial h a)
deduce opx opyxT opyxF = do
  opx'   <- toBinomial <$> opx
  opyxT' <- toBinomial <$> opyxT
  opyxF' <- toBinomial <$> opyxF
  let ay  = bAtomicity opyxT'
      ay' = bAtomicity opyxF'
  require (ay == ay') "y's atomicity must be the same."
  pure $ deduce' opx' opyxT' opyxF'

deduce' :: Binomial h a -> Binomial h a -> Binomial h a -> Binomial h a
deduce' opx opy opy' = Binomial byx dyx uyx ayx undefined
  where
    (Binomial bx dx ux ax _)     = opx
    (Binomial by dy uy ay _)     = opy
    (Binomial by' dy' uy' ay' _) = opy'

    byx = bIy - ay * k
    dyx = dIy - (1 - ay) * k
    uyx = uIy + k
    ayx = ay

    bIy = bx * by + dx * by' + ux * (by * ax + by' * (1 - ax))
    dIy = bx * dy + dx * dy' + ux * (dy * ax + dy' * (1 - ax))
    uIy = bx * uy + dx * uy' + ux * (uy * ax + uy' * (1 - ax))

    k | ((by > by') && (dy > dy')) || ((by <= by') && (dy <= dy')) = 0

      | ((by > by') && (dy <= dy')) && b1 && a1 = k2a1
      | ((by > by') && (dy <= dy')) && b1 && a2 = k2a2
      | ((by > by') && (dy <= dy')) && b2 && a1 = k2b1
      | ((by > by') && (dy <= dy')) && b2 && a2 = k2b2

      | ((by <= by') && (dy > dy')) && b1' && a1 = k3a1
      | ((by <= by') && (dy > dy')) && b1' && a2 = k3a2
      | ((by <= by') && (dy > dy')) && b2' && a1 = k3b1
      | ((by <= by') && (dy > dy')) && b2' && a2 = k3b2

    a1 = (eopx <= ax)
    a2 = (eopx > ax)

    b1  = (eopy' <= (by' + ay * (1 - by' - dy)))
    b1' = (eopy' <= (by  + ay * (1 - by  - dy')))
    b2  = (eopy' >  (by' + ay * (1 - by' - dy)))
    b2' = (eopy' >  (by  + ay * (1 - by  - dy')))

    k2a1 = (ax * ux * (bIy - by')) / ((bx + ax * ux) * ay)
    k2a2 = (ax * ux * (dIy - dy) * (by - by')) / ((dx + (1 - ax) * ux) * ay * (dy' - dy))
    k2b1 = ((1 - ax) * ux * (bIy - by') * (dy' - dy)) / ((bx + ax * ux) * (1 - ay) * (by - by'))
    k2b2 = ((1 - ax) * ux * (dIy - dy)) / ((dx + (1 - ax) * ux) * (1 - ay))

    k3a1 = ((1 - ax) * ux * (dIy - dy') * (by' - by)) / ((bx + ax * ux) * ay * (dy - dy'))
    k3a2 = ((1 - ax) * ux * (bIy - by)) / ((dx + (1 - ax) * ux) * ay)
    k3b1 = (ax * ux * (dIy - dy')) / ((bx + ax * ux) * (1 - ay))
    k3b2 = (ax * ux * (bIy - by) * (dy - dy')) / ((dx + (1 - ax) * ux) * (1 - ay) * (by' - by))

    eopx  = expectation opx
    eopy' = by * ax + by' * (1 - ax) + ay * (uy * ax + uy' * (1 - ax))
\end{code}



\begin{code}
abduce :: (ToBinomial op1, ToBinomial op2, ToBinomial op3)
          => SLExpr h a (op1 h a)
          -> SLExpr h a (op2 h a)
          -> SLExpr h a (op3 h a)
          -> SLExpr h a Rational
          -> SLExpr h a (Binomial h a)
abduce opx opxyT opxyF ay = do
  opx'   <- toBinomial <$> opx
  opxyT' <- toBinomial <$> opxyT
  opxyF' <- toBinomial <$> opxyF
  ay'    <- ay
  abduce' opx' opxyT' opxyF' ay'



abduce' :: Binomial h a
           -> Binomial h a
           -> Binomial h a
           -> Rational
           -> SLExpr h a (Binomial h a)
abduce' opx opxyT opxyF ay = deduce (pure opx) opyxT opyxF
  where
    (Binomial bx dx ux ax _)         = opx
    (Binomial bxy dxy uxy axy _)     = opxyT
    (Binomial bxy' dxy' uxy' axy' _) = opxyF

    -- 4.58
    e_opxyT = bxy + ax * uxy
    e_opxyF = bxy' + ax * uxy'

    -- 4.59
    e_opyxT = (ay * e_opxyT) / (ay * e_opxyT + (1 - ay) * e_opxyF)
    e_opyxF = (ay * (1 - e_opxyT)) / (ay * (1 - e_opxyT) + (1 - ay) * (1 - e_opxyF))

    relevance = undefined :: Rational

    -- For now...
    opyxT = pure opxyT
    opyxF = pure opxyF
\end{code}







\end{document}
