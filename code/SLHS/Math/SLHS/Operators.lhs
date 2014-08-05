\documentclass[thesis.tex]{subfiles}

\begin{document}


\section{Operators}


In this section we will discuss the implementation details of the
Subjective Logic operators that are provided by SLHS. The following
notation is used for the operators:

\begin{itemize}
  \item We denote binary operators with a trailing exclamation mark
    $!$ in order to avoid conflicting with Haskell's mathematical
    operators. For example, binomial addition is denoted as $+!$.
  \item We use tildas as a prefix to denote $co-$ operations. For
    example, the binomial co-multiplication operator is denoted as
    $\sim *!$.
  \item All n-ary operators, where $n > 2$ are denoted as simple
    functions, instead of symbolic operators.
\end{itemize}

Furthermore, every operator is presented in it's most general
form. For example, instead of presenting two operators for
\emph{averaging fusion} (one for multinomial opinions, and another for
hyper opinions) we implement only the version for hyper opinions.


\ignore{
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.SLHS.Operators where

import Control.Monad (liftM2, join)
import Control.Applicative
import Data.List

import Math.SLHS.Types
import Math.SLHS.Opinions

import qualified Math.SLHS.Frame as F
import qualified Math.SLHS.Vector as V
\end{code}
}


\subsection{Binomial Operators}


We begin our treatment of the Subjective Logic operators by looking at
those operators designed to work with the simplest of objects:
binomial opinions. Binomial operators are split into three primary
sections: \emph{logical and set-theoretical operators} are those which
have analogs to binary logical (conjunction, disjunction) or set
theoretical (union, difference) operations, \emph{trust transitivity
operators} are operators that deal with modelling transitive trust
networks between agents, and \emph{reasoning operators} support
deductive and abductive reasoning.


\subsubsection{Logical and Set-Theoretical Operators}


We will begin with the simplest of binomial operators: those of
addition and subtraction.  Addition of two binomial opinions
corresponds to the set theoretic union operator. Given two binomial
opinions representing disjoint subsets of the same frame, the sum of
the two opinions represents the union of their respective subsets.


\begin{code}
(+!) :: (ToBinomial op1, ToBinomial op2)
       => SLExpr h a (op1 h (F.Subframe a))
       -> SLExpr h a (op2 h (F.Subframe a))
       -> SLExpr h a (Binomial h (F.Subframe a))
opx +! opy = pure add' <*> (toBinomial <$> opx) <*> (toBinomial <$> opy)
\end{code}


\begin{code}
add' :: Binomial h (F.Subframe a) -> Binomial h (F.Subframe a) -> Binomial h (F.Subframe a)
add' (Binomial bx dx ux ax _) (Binomial by dy uy ay _) =
  Binomial b' d' u' a' undefined
  where
    b' = bx + by
    d' = (ax * (dx - by) + ay * (dy - bx)) / (ax + ay)
    u' = (ax * ux + ay * uy) / (ax + ay)
    a' = ax + ay
\end{code}


Binomial subtraction is the inverse operation of addition. It is equivalent to
the set difference operator, and is defined as follows.


\begin{code}
(-!) :: (ToBinomial op1, ToBinomial op2)
        => SLExpr h a (op1 h (F.Subframe a))
        -> SLExpr h a (op2 h (F.Subframe a))
        -> SLExpr h a (Binomial h (F.Subframe a))
opx -! opy = pure subtract' <*> (toBinomial <$> opx) <*> (toBinomial <$> opy)
\end{code}


\begin{code}
subtract' (Binomial bx dx ux ax _) (Binomial by dy uy ay _) =
  Binomial b' d' u' a' undefined
  where
    b' = bx - by
    d' = (ax * (dx + by) - ay * (1 + by - bx - uy)) / (ax - ay)
    u' = (ax * ux - ay * uy) / (ax - ay)
    a' = ax - ay
\end{code}


Negation is a unary operator that inverts the belief and disbelief and
atomicity of a binomial opinion. Given a binomial opinion $\omega_x$ over
a frame $X = \lbrace x, \lnot x \rbrace$, the negated opinion
$\lnot \omega_x = \omega_{\lnot x}$.


\begin{code}
negate :: ToBinomial op => SLExpr h a (op h a) -> SLExpr h a (Binomial h a)
negate op = pure negate' <*> (toBinomial <$> op)
\end{code}


\begin{code}
negate' :: Binomial h a -> Binomial h a
negate' (Binomial b d u a _) = Binomial d b u (1 - a) undefined
\end{code}


Multiplication of two binomial opinions is equivalent to the logical
\emph{and} operator.


\begin{code}
(*!) :: (ToBinomial op1, ToBinomial op2)
        => SLExpr h a (op1 h a)
        -> SLExpr h a (op2 h a)
        -> SLExpr h a (Binomial h (F.Subframe (a, a)))
opx *! opy = pure b_times' <*> (toBinomial <$> opx) <*> (toBinomial <$> opy)


b_times' (Binomial bx dx ux ax _) (Binomial by dy uy ay _) =
  Binomial b' d' u' a' undefined
  where
    b' = bx * by + ((1 - ax) * bx * uy + (1 - ay) * ux * by)
         / (1 - ax * ay)
    d' = dx + dy - dx * dy
    u' = ux * uy + ((1 - ay) * bx * uy + (1 - ax) * ux * by)
         / (1 - ax * ay)
    a' = ax * ay
\end{code}


... and co-multiplication is equivalent to the logical \emph{or} operator.


\begin{code}
(~*!) :: (ToBinomial op1, ToBinomial op2)
         => SLExpr h a (op1 h a)
         -> SLExpr h a (op2 h a)
         -> SLExpr h a (Binomial h (F.Subframe (a, a)))
opx ~*! opy = do opx' <- toBinomial <$> opx
                 opy' <- toBinomial <$> opy
                 pure $ cotimes' opx' opy'

cotimes' (Binomial bx dx ux ax _) (Binomial by dy uy ay _) =
  Binomial b' d' u' a' undefined
  where
    b' = bx + by - bx * by
    d' = dx * dy + (ax * (1 - ay) * dx * uy + (1 - ax) * ay * ux * dy)
         / (ax + ay - ax * ay)
    u' = ux * uy + (ay * dx * uy + ax * ux * dy)
         / (ax + ay - ax * ay)
    a' = ax + ay - ax * ay
\end{code}


Division is the inverse of multiplication.


\begin{code}
(/!) :: (ToBinomial op1, ToBinomial op2)
        => SLExpr h a (op1 h (F.Subframe (a, a)))
        -> SLExpr h a (op2 h a)
        -> SLExpr h a (Binomial h a)
opx /! opy = do opx' <- toBinomial <$> opx
                opy' <- toBinomial <$> opy
                pure $ divide' opx' opy'

divide' (Binomial bx dx ux ax _) (Binomial by dy uy ay _) =
  Binomial b' d' u' a' undefined
  where
    b' = ay * (bx + ax * ux) / ((ay - ax) * (by + ay *uy))
         - ax * (1 - dx) / ((ay - ax) * (1 - dy))
    d' = (dx - dy) / (1 - dy)
    u' = ay * (1 - dx) / ((ay - ax) * (1 - dy))
         - ay * (bx + ax * ux) / ((ay - ax) * (bx + ay * uy))
    a' = ax / ay
\end{code}


And similarily, co-division is the inverse of co-multiplication.


\begin{code}
(~/!) :: (ToBinomial op1, ToBinomial op2)
         => SLExpr h a (op1 h (F.Subframe (a, a)))
         -> SLExpr h a (op2 h a)
         -> SLExpr h a (Binomial h a)
opx ~/! opy = do opx' <- toBinomial <$> opx
                 opy' <- toBinomial <$> opy
                 pure $ codivide' opx' opy'

codivide' (Binomial bx dx ux ax _) (Binomial by dy uy ay _) =
  Binomial b' d' u' a' undefined
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















\subsubsection{Trust Transitivity Operators}

In this section we implement the subjective logic operators for trust
transitivity. If two agents A and B exist such that A has an opinion
about B's recommendation of some proposition x, then A can generate an
opinion about x by \emph{discounting} B's recommendation of x based on
A's opinion of B.

Subjective Logic offers three methods of discounting:
\emph{uncertainty favouring discounting}, \emph{opposite belief
favouring discounting}, and \emph{base rate sensitive discounting}.

We begin by constructing a simple algebraic data type to represent
each of the three kinds of discounting.


\begin{code}
data Favouring = Uncertainty | Opposite | BaseRateSensitive
\end{code}


By doing so, we are able to expose a single discounting function to
the user with the following signature:

\begin{spec}
discount :: Favouring -> SLExpr a (Binomial a) -> SLExpr a (Binomial a) -> SLExpr a (Binomial a)
\end{spec}

Since the arrow operator for function signatures is right associative,
one can consider \emph{discount} to be a function mapping discount
favourings to a binary function over binomial opinions much like the
operators discussed in the previous section.

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






%\subsubsection{Reasoning Operators}


\ignore{
\begin{code}
{-
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
-}
\end{code}
}


\ignore{
\begin{code}
{-
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
-}
\end{code}
}


The binomial operators are summarized in table \ref{tbl:binomial-operators}.

\begin{table}
\begin{center}
\begin{tabular}{| l | l | l |}
  \hline
  Name & SL & SLHS \\
  \hline
  Addition          & $\omega_{X \cup Y} = \omega_X + \omega_Y$       & $opx +! opy$  \\
  Subtraction       & $\omega_{X \setminus Y} = \omega_X - \omega_Y$  & $opx -! opy$  \\
  Negation          & $\omega_{X \setminus Y} = \omega_X - \omega_Y$  & $opx -! opy$  \\
  Multiplication    & $\omega_{X \land Y} = \omega_X \times \omega_Y$ & $opx *! opy$  \\
  Co-multiplication & $\omega_{X \land Y} = \omega_X \times \omega_Y$ & $opx ~*! opy$ \\
  Division          & $\omega_{X \land Y} = \omega_X \times \omega_Y$ & $opx /! opy$  \\
  Co-division       & $\omega_{X \land Y} = \omega_X \times \omega_Y$ & $opx ~/! opy$ \\
  \hline
\end{tabular}
\end{center}

\caption{Summary of binomial operators}
\label{tbl:binomial-operators}
\end{table}




\subsection{Multinomial and Hyper Operators}


\subsubsection{Multinomial Multiplication}

The multiplication of two multinomial opinions is a separate operator
than the product operator defined over binomial opinions. Whereas the
binomial product operator is equivalent to the logical \emph{and}
operator, multinomial multiplication constructs an opinion over a new
frame which is the cartesian product of the frames of the input opinions.
In order to avoid naming conflicts, we have chosen to name the binomial
operator with the symbol $*!$, and we use the name \emph{times} to
denote the multinomial operator.


\begin{code}
times :: (ToMultinomial op1, ToMultinomial op2, Ord b, Ord c)
         => SLExpr h a (op1 h b) -> SLExpr h a (op2 h c)
         -> SLExpr h a (Multinomial h (b, c))
times opx opy = do opx' <- toMultinomial <$> opx
                   opy' <- toMultinomial <$> opy
                   return $ m_times' opx' opy'
\end{code}



\begin{code}
m_times' :: (Ord a, Ord b) => Multinomial h a -> Multinomial h b -> Multinomial h (a, b)
m_times' (Multinomial bx ux ax _) (Multinomial by uy ay _) =
  Multinomial b' u' a' undefined
  where
    b' = V.fromList bxy
    u' = uxy
    a' = V.fromList axy

    bxy = [ ((x, y), f x y) | x <- xKeys, y <- yKeys ]
      where
        f x y = expect x y - V.value ax x * V.value ay y * uxy

    uxy = minimum [ uxy' x y | x <- xKeys, y <- yKeys ]

    axy = [ ((x, y), f x y) | x <- xKeys, y <- yKeys ]
      where
        f x y = V.value ax x * V.value ay y

    uxy' x y = uIxy * expect x y /
               (bIxy x y + V.value ax x * V.value ay y * uIxy)

    uIxy = uRxy + uCxy + uFxy
      where
        uRxy = 1 - ux * sum [ V.value by y | y <- yKeys ]
        uCxy = 1 - uy * sum [ V.value bx x | x <- xKeys ]
        uFxy = ux * uy

    bIxy x y = V.value bx x * V.value by y

    expect x y = (V.value bx x + V.value ax x * ux) *
                 (V.value by y + V.value ay y * uy)

    -- Be careful with these. Think this through...
    xKeys = nub $ V.focals bx ++ V.focals ax
    yKeys = nub $ V.focals by ++ V.focals ay
\end{code}


\subsubsection{Fusion, Unfusion, and Fission}

Hyper opinions can be fused together using two different operators:
\emph{cumulative fusion} and \emph{averaging fusion}. Each operator
should be used under different circumstances depending on the meaning
of the fused opinions.

We first introduce the cumulative fusion operator:

\begin{code}
cFuse :: (ToHyper op1, ToHyper op2, Ord a)
         => SLExpr h a (op1 h a) -> SLExpr h a (op2 h a) -> SLExpr h a (Hyper h a)
cFuse opa opb = do opa' <- toHyper <$> opa
                   opb' <- toHyper <$> opb
                   pure $ cFuse' opa' opb'
\end{code}

\begin{code}
cFuse' :: Ord a => Hyper h a -> Hyper h a -> Hyper h a
cFuse' (Hyper ba ua aa _) (Hyper bb ub ab _)
  | ua /= 0 || ub /= 0 = Hyper b' u' a' undefined
  | otherwise          = Hyper b'' u'' a'' undefined
  where
    b' = V.fromList . map (\k -> (k, bFunc k)) $ keys
    u' = ua * ub / (ua + ub - ua * ub)
    a' = aa

    b'' = V.fromList . map (\k -> (k, bB k)) $ keys
    u'' = 0
    a'' = aa

    bFunc x = (bA x * ub + bB x * ua) / (ua + ub - ua * ub)

    keys  = nub (V.focals ba ++ V.focals bb)

    bA = V.value ba
    bB = V.value bb
\end{code}

And secondly, the averaging fusion operator:

\begin{code}
aFuse :: (ToHyper op1, ToHyper op2, Ord a)
         => SLExpr h a (op1 h a) -> SLExpr h a (op2 h a) -> SLExpr h a (Hyper h a)
aFuse opa opb = do opa' <- toHyper <$> opa
                   opb' <- toHyper <$> opb
                   pure $ aFuse' opa' opb'
\end{code}

\begin{code}
aFuse' :: Ord a => Hyper h a -> Hyper h a -> Hyper h a
aFuse' (Hyper ba ua aa _) (Hyper bb ub ab _)
  | ua /= 0 || ub /= 0 = Hyper b' u' a' undefined
  | otherwise          = Hyper b'' u'' a'' undefined
  where
    b' = V.fromList . map (\k -> (k, bFunc k)) $ keys
    u' = 2 * ua * ub / (ua + ub)
    a' = aa

    b'' = V.fromList . map (\k -> (k, bB k)) $ keys
    u'' = 0
    a'' = aa

    bFunc x = (bA x * ub + bB x * ua) / (ua + ub)

    keys  = nub (V.focals ba ++ V.focals bb)

    bA = V.value ba
    bB = V.value bb
\end{code}

Cumulative \emph{unfusion} is defined for multinomial opinions. It has
yet to be generalized to hyper opinions. Given an opinion that represents
the result of cumulatively fusing together two opinions, and one of the
two original opinions, it is possible to extract the other original
opinion.

\begin{code}
cUnfuse :: (ToMultinomial op1, ToMultinomial op2, Ord a)
           => SLExpr h a (op1 h a) -> SLExpr h a (op2 h a)
           -> SLExpr h a (Multinomial h a)
cUnfuse opc opb = do opc' <- toMultinomial <$> opc
                     opb' <- toMultinomial <$> opb
                     pure $ cUnfuse' opc' opb'
\end{code}

\begin{code}
cUnfuse' :: Ord a => Multinomial h a -> Multinomial h a -> Multinomial h a
cUnfuse' (Multinomial bc uc ac _) (Multinomial bb ub ab _)
  | uc /= 0 || ub /= 0 = Multinomial ba ua aa undefined
  | otherwise          = Multinomial ba' ua' aa' undefined
  where
    ba = V.mapWithKey belief bc
    ua = ub * uc / (ub - uc + ub * uc)
    aa = ac

    ba' = bb
    ua' = 0
    aa' = ac

    belief x b = (b * ub - V.value bb x  * uc) / (ub - uc + ub * uc)
\end{code}

Likewise, averaging unfusion is the inverse operation to averaging fusion.


\begin{code}
aUnfuse :: (ToMultinomial op1, ToMultinomial op2, Ord a)
           => SLExpr h a (op1 h a) -> SLExpr h a (op2 h a)
           -> SLExpr h a (Multinomial h a)
aUnfuse opc opb = do opc' <- toMultinomial <$> opc
                     opb' <- toMultinomial <$> opb
                     pure $ aUnfuse' opc' opb'
\end{code}

\begin{code}
aUnfuse' :: Ord a => Multinomial h a -> Multinomial h a -> Multinomial h a
aUnfuse' (Multinomial bc uc ac _) (Multinomial bb ub ab _)
  | uc /= 0 || ub /= 0 = Multinomial ba ua aa undefined
  | otherwise          = Multinomial ba' ua' aa' undefined
  where
    ba = V.mapWithKey belief bc
    ua = ub * uc / (2 * ub - uc)
    aa = ac

    ba' = bb
    ua' = 0
    aa' = ac

    belief x b = (2 * b * ub - V.value bb x * uc) / (2 * ub - uc)
\end{code}

Fission is the operation of splitting a multinomial opinion into two
multinomial opinions based on some ratio $\phi$. We refer to this as
the \emph{split} operator. Like unfusion, fission has not yet been
generalized to hyper opinions.

\begin{code}
cSplit :: ToMultinomial op => Rational -> SLExpr h a (op h a)
          -> SLExpr h a (Multinomial h a , Multinomial h a)
cSplit phi op = cSplit' <$> (pure phi) <*> (toMultinomial <$> op)
\end{code}


\begin{code}
cSplit' :: Rational -> Multinomial h a -> (Multinomial h a, Multinomial h a)
cSplit' phi (Multinomial b u a _) = (op1, op2)
  where
    op1 = Multinomial b1 u1 a undefined
    op2 = Multinomial b2 u2 a undefined

    b1 = V.map (\x -> phi * x / norm phi) b
    u1 = u / norm phi

    b2 = V.map (\x -> (1 - phi) * x / norm (1 - phi)) b
    u2 = u / norm (1 - phi)

    norm p = u + p * V.fold (+) 0 b
\end{code}


\subsubsection{Deduction and Abduction}

Deduction and abduction of multinomial opinions allows for one to do
conditional reasoning with Subjective Logic. We first introduce the
operator for performing deduction, which we call \emph{deduce}, and
then discuss the operator \emph{abduce} for performing abduction.

Because of the nature of these operators, the frames of discernment
which the opinions are defined over must satisfy two properties: they
must be \emph{bounded}, and the must be \emph{enumerable}. These
constraints on the type of frames allowed is expressed via the type
classes \emph{Bounded} and \emph{Enum}. Boundedness simply means that
there exists a least and greatest element, and enumerability insists
that the frames be mappable to the integers.

We will begin by introducing deduction. Subjective Logic deduction is
a fairly complex operation, and so we have split the implementation
into steps, following it's description (cite).


\begin{code}
deduce :: (ToMultinomial op, Ord a, Bounded a, Enum a, Ord b, Bounded b, Enum b)
          => SLExpr h a (op h a)
          -> [(a, Multinomial h b)]
          -> SLExpr h a (Multinomial h b)
deduce opx ops = do opx' <- toMultinomial <$> opx
                    pure $ deduce' opx' ops
\end{code}


\begin{code}
deduce' :: forall a. forall b. forall h.
           (Ord a, Bounded a, Enum a, Ord b, Bounded b, Enum b)
           => Multinomial h a
           -> [(a, Multinomial h b)]
           -> Multinomial h b
deduce' opx@(Multinomial bx ux ax _) ops = Multinomial b' u' a' undefined
  where
\end{code}

Some helper functions.

The expectation of y given a vacuous expectation on X.

\begin{code}
    expt y = sum . map f $ xs
      where
        f x = V.value ax x * V.value (expectation (findOpinion x)) y
\end{code}

The expectation of y given X.

\begin{code}
    expt' y = sum . map f $ xs
      where
        f x = V.value (expectation opx) x * V.value (expectation (findOpinion x)) y
\end{code}

The expectation of y given the theoretical maximum uncertainty.

\begin{code}
    tExpt y = (1 - V.value ay y) * byxs + (V.value ay y) * (byxr + uyxr)
      where
        (xr', xs') = dims y
        byxr       = V.value (mBelief xr') y
        uyxr       = mUncertainty xr'
        byxs       = V.value (mBelief xs') y
\end{code}

All values of frames X and Y. We need to enumerate them in their entirity.

\begin{code}
    xs = [minBound .. maxBound] :: [a]
    ys = [minBound .. maxBound] :: [b]
\end{code}

All of the base rate vectors must be the same, so take the first one.

\begin{code}
    ay = mBaseRate . snd . head $ ops
\end{code}

Next, some helper functions.

\begin{code}
    uYx x = maybe 1 mUncertainty . lookup x $ ops

    findOpinion x = case lookup x ops of
      Nothing -> Multinomial (V.fromList []) 1 ay undefined
      Just op -> op
\end{code}

Step 1:
For each y, we compute the pair (xr, xs) that yields the theoretical
maximum uncertainty.

\begin{code}
    dims :: b -> (Multinomial h b, Multinomial h b)
    dims y = (xr', xs')
      where
        (_, xr', xs') = foldl1' minPair (dims' y)
        minPair a@(u, _, _) b@(u', _, _) | u < u'    = a
                                         | otherwise = b

        dims' y = do xr' <- xs
                     xs' <- xs
                     let xr'' = findOpinion xr'
                         xs'' = findOpinion xs'
                         byxr = V.value (mBelief xr'') y
                         uyxr = mUncertainty xr''
                         byxs = V.value (mBelief xs'') y
                         val  = 1 - byxr - uyxr + byxs
                     return (val, xr'', xs'')
\end{code}

Step 2: Compute the triangle apex uncertainty for each y.

\begin{code}
    triangleApexU y
      | expt y <= tExpt y = (expt y - byxs) / V.value ay y
      | otherwise         = (byxr + uyxr - expt y) / (1 - V.value ay y)
      where
        byxr = V.value (mBelief . fst . dims $ y) y
        uyxr = mUncertainty . fst . dims $ y
        byxs = V.value (mBelief . snd . dims $ y) y
\end{code}

Intermediate sub-simplex apex uncertainty.

\begin{code}
    intApexU = maximum . map triangleApexU $ ys
\end{code}

Step 3: Compute the intermediate belief components.

\begin{code}
    bComp y = expt y - V.value ay y * intApexU
\end{code}

Compute the adjusted apex uncertainty.

\begin{code}
    adjustedU y | bComp y < 0 = expt y / V.value ay y
                | otherwise   = intApexU
\end{code}

Finally compute the sub-simplex apex uncertainty.

\begin{code}
    apexU = minimum . map adjustedU $ ys
\end{code}

Step 4: The final values we expect.

\begin{code}
    b' = V.fromList [ (y, expt' y - (V.value ay y) * u') | y <- ys ]
    u' = (apexU -) . sum . map (\x -> (apexU - uYx x) * V.value bx x) $ xs
    a' = ay
\end{code}


Subjective Logic abduction is a two step procedure. Given an opinion
over a frame X and a list of conditional opinions over X given Y, we
first must invert the conditionals into a list of conditional opinions
over Y given X, and then perform Subjective Logic deduction with the
new list and the opinion over X. Failure to invert the conditional
opinions when performing abductive reasoning results in one falling
victim to the \emph{prosecutor's fallacy}.


\begin{code}
abduce :: (ToMultinomial op,
           Ord a, Bounded a, Enum a,
           Ord b, Bounded b, Enum b)
          => SLExpr h a (op h a)
          -> [(b, Multinomial h a)]
          -> BaseRateVector b
          -> SLExpr h a (Multinomial h b)
abduce opx ops ay = do opx' <- toMultinomial <$> opx
                       pure $ abduce' opx' ops ay
\end{code}


\begin{code}
abduce' :: forall a. forall b. forall h.
           (Ord a, Bounded a, Enum a, Ord b, Bounded b, Enum b)
           => Multinomial h a
           -> [(b, Multinomial h a)]
           -> BaseRateVector b
           -> Multinomial h b
abduce' opx@(Multinomial bx ux ax _) ops ay = deduce' opx ops'
  where
\end{code}

\begin{code}
    ops' = map multinomial xs

    multinomial x = (x, Multinomial b' u' a' undefined)
      where
        b'  = V.fromList bs
        u'  = uT x
        a'  = ay
        bs  = map (\y -> (y, f y)) ys
        f y = expt y x - V.value ay y * uT x
\end{code}

\begin{code}
    expt y x = numer / denom
      where
        numer = V.value ay y * V.value (expectation (findOpinion y)) x
        denom = sum . map f $ ys
        f y   = V.value ay y  * V.value (expectation (findOpinion y)) x
\end{code}

\begin{code}
    uT x = minimum . map f $ ys
      where
        f y = expt y x / V.value ay y
\end{code}

TODO: Confirm this.

\begin{code}
    ax = mBaseRate . snd . head $ ops
\end{code}

\begin{code}
    xs = [minBound .. maxBound] :: [a]
    ys = [minBound .. maxBound] :: [b]
\end{code}

\begin{code}
    -- TODO: Factor this out.
    findOpinion y = case lookup y ops of
      Nothing -> Multinomial (V.fromList []) 1 ax undefined
      Just op -> op
\end{code}


\subsubsection{Belief Constraining}

The final operator we will discuss is the \emph{belief constraint}
operator. This operator takes as input two objects that are convertible
to hyper opinions and returns a hyper opinion as output. This function
is equivalent in meaning to Dempster's rule of combination from
\emph{Dempster Shafer Theory}.


\begin{code}
constraint :: (ToHyper op1, ToHyper op2, Ord a)
              => SLExpr h a (op1 h a)
              -> SLExpr h a (op2 h a)
              -> SLExpr h a (Hyper h a)
constraint op1 op2 = constraint' <$> (fmap toHyper op1) <*> (fmap toHyper op2)
\end{code}


\begin{code}
constraint' :: Ord a => Hyper h a -> Hyper h a -> Hyper h a
constraint' (Hyper bA uA aA _) (Hyper bB uB aB _) = Hyper bAB uAB aAB undefined
  where
    bAB = V.fromList . map (\k -> (k, harmony k / (1 - conflict))) $ keys
    uAB = (uA * uB) / (1 - conflict)
    aAB = V.fromList $ map (\k -> (k, f k)) keys'
      where
        f x = (axA * (1 - uA) + axB * (1 - uB)) / (2 - uA - uB)
          where
            axA = V.value aA x
            axB = V.value aB x

    harmony x = bxA * uB + bxB * uA + rest
      where
        bxA     = V.value bA x
        bxB     = V.value bB x
        rest    = sum . map combine $ matches
        matches = [(y, z) | y <- keys, z <- keys, F.union y z == x]

    conflict = sum . map combine $ matches
      where
        matches = [(y, z) | y <- keys, z <- keys, F.union y z == F.empty]

    combine (y, z) = V.value bA y + V.value bB z

    keys  = nub (V.focals bA ++ V.focals bB)
    keys' = nub (V.focals aA ++ V.focals aB)
\end{code}


\end{document}
