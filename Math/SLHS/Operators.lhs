\documentclass[thesis.tex]{subfiles}

\begin{document}


\section{Operators}

In this section we discuss the implementation details of the
Subjective Logic operators that are provided by SLHS. The following
notation is used for the operators:

\begin{itemize}
  \item We denote binary operators with a trailing exclamation mark
    $!$ in order to avoid conflicting with Haskell's mathematical
    operators. For example, binomial addition is denoted as $+!$.
  \item We use tildes as a prefix to denote $co-$ operations. For
    example, the binomial co-multiplication operator is denoted as
    $\sim *!$.
  \item All n-ary operators, where $n > 2$ are denoted as simple
    functions, instead of symbolic operators.
\end{itemize}

Every operator is presented in its most general
form. For example, instead of presenting two operators for
\emph{averaging fusion} (one for multinomial opinions, and another for
hyper opinions) we implement only the version for hyper opinions. In
order to achieve this level of code reuse, each operator accepts
as parameters any object that can be converted into the correct
opinion type by virtue of the \emph{ToBinomial}, \emph{ToMultinomial},
and \emph{ToHyper} type classes.


\ignore{
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.SLHS.Operators where

import Data.Ratio ((%))
import Control.Monad
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
those operators designed to work with binomial opinions. We split this
section into two parts: \emph{logical and set-theoretical} operators, and
\emph{trust transitivity} operators. The former contains the operators
that are generalizations of those found in logic and set theory, such
as conjunction, and set union. The latter operators are for modeling
trust networks, where agents can formulate opinions based on reputation
and trust.


\subsubsection{Logical and Set-Theoretical Operators}

The logical and set-theoretical binomial operators are those that have
equivalent operators in logic and set theory. We will start with binomial addition.
Addition of binomial opinions, denoted as $\omega_{x \cup y} = \omega_x + \omega_y$,
is defined when $x$ and $y$ are disjoint subsets of the same frame of discernment
\cite{mcanally2004addition}. Binomial addition is implemented as follows:

\begin{code}
(+!) :: (ToBinomial op1, ToBinomial op2, Eq h, Eq b, Ord b)
       => SLExpr h a (op1 h (F.Frame b))
       -> SLExpr h a (op2 h (F.Frame b))
       -> SLExpr h a (Binomial h (F.Frame b))
opx +! opy = do
  opx' <- liftM toBinomial opx
  opy' <- liftM toBinomial opy
  require (bHolder opx' == bHolder opy') "opinions must have same holder"
  require (getFrame opx' == getFrame opy') "opinions must have the same frame"
  return $ add' opx' opy'
\end{code}

\begin{code}
add' :: Ord a
        => Binomial h (F.Frame a) -> Binomial h (F.Frame a) -> Binomial h (F.Frame a)
add' opx@(Binomial bx dx ux ax hx xt xf) (Binomial by dy uy ay _ yt yf) =
  Binomial b' d' u' a' hx (xt `F.union` yt) (xf `F.union` yf)
  where
    b' = bx + by
    d' = (ax * (dx - by) + ay * (dy - bx)) / (ax + ay)
    u' = (ax * ux + ay * uy) / (ax + ay)
    a' = ax + ay
\end{code}

Here we see a pattern that we will re-use for all operator implementations. We start
with a function whose inputs are of type \emph{SLExpr h a t}, where $t$ is some
type. Within that function, we unwrap the values from the \emph{SLExpr} monad, verify
that some requirements are met, and then send those values to a worker function that
does the actual computation. We then wrap the result back into the \emph{SLExpr}
monad via the \emph{return} function.

Binomial subtraction is the inverse operation of addition. In set theory it is
equivalent to the set difference operator \cite{mcanally2004addition}. Given
two opinions $\omega_x$ and $\omega_y$ where $x \cap y = y$, the difference,
$\omega_{x \setminus y}$ is calculated as follows:

\begin{code}
(-!) :: (ToBinomial op1, ToBinomial op2, Eq h, Eq b, Ord b)
        => SLExpr h a (op1 h (F.Frame b))
        -> SLExpr h a (op2 h (F.Frame b))
        -> SLExpr h a (Binomial h (F.Frame b))
opx -! opy = do
  opx' <- liftM toBinomial opx
  opy' <- liftM toBinomial opy
  require (bHolder opx' == bHolder opy') "opinions must have same holder"
  require (getFrame opx' == getFrame opy') "opinions must have the same frame"
  return $ subtract' opx' opy'
\end{code}

\begin{code}
subtract' :: Ord a
             => Binomial h (F.Frame a) -> Binomial h (F.Frame a) -> Binomial h (F.Frame a)
subtract' (Binomial bx dx ux ax hx xt xf) (Binomial by dy uy ay _ yt yf) =
  Binomial b' d' u' a' hx ft ff
  where
    b' = bx - by
    d' = (ax * (dx + by) - ay * (1 + by - bx - uy)) / (ax - ay)
    u' = (ax * ux - ay * uy) / (ax - ay)
    a' = ax - ay
    ft = xt `F.difference` yt
    ff = xt `F.union` xf `F.difference` ft
\end{code}

Negation is a unary operator that switches the belief and disbelief and
inverts the atomicity of a binomial opinion \cite{josang2001logic}.
Given a binomial opinion $\omega_x$ over
a frame $X = \lbrace x, \lnot x \rbrace$, the negated opinion
$\omega_{\overline{x}} = \omega_{\lnot x}$.

\begin{code}
negate :: ToBinomial op => SLExpr h a (op h b) -> SLExpr h a (Binomial h b)
negate op = do
  op' <- liftM toBinomial op
  return $ negate' op'
\end{code}

\begin{code}
negate' :: Binomial h a -> Binomial h a
negate' (Binomial b d u a h x y) = Binomial d b u (1 - a) h y x
\end{code}

Multiplication of two binomial opinions is equivalent to the logical
\emph{and} operator \cite{josang2005multiplication}. Given two opinions
$\omega_x$ and $\omega_y$ over distinct binary frames $x$ and $y$, the
product of the opinions, $\omega_{x \land y}$, represents the conjunction
of the two opinions.

\begin{code}
(*!) :: (ToBinomial op1, ToBinomial op2, Eq h, Ord b, Ord c)
        => SLExpr h a (op1 h b)
        -> SLExpr h a (op2 h c)
        -> SLExpr h a (Binomial h (F.Frame (b, c)))
opx *! opy = do
  opx' <- liftM toBinomial opx
  opy' <- liftM toBinomial opy
  require (bHolder opx' == bHolder opy') "opinions must have same holder"
  return $ b_times' opx' opy'

b_times' (Binomial bx dx ux ax hx xt xf) (Binomial by dy uy ay _ yt yf) =
  Binomial b' d' u' a' hx t f
  where
    b' = bx * by + ((1 - ax) * bx * uy + (1 - ay) * ux * by)
         / (1 - ax * ay)
    d' = dx + dy - dx * dy
    u' = ux * uy + ((1 - ay) * bx * uy + (1 - ax) * ux * by)
         / (1 - ax * ay)
    a' = ax * ay

    t = F.singleton (xt, yt)
    f = F.fromList [(xt, yf), (xf, yt), (xf, yf)]
\end{code}

The resulting frame of discernment is a coarsened frame from the
cartesian product of $\lbrace x, \lnot x\rbrace$ and
$\lbrace y, \lnot y\rbrace$, where the element whose belief mass is
designated the role of "belief" for binomial opinions is
$\lbrace (x, y)\rbrace$, and the element whose belief mass is given
the role of "disbelief" is $\lbrace (x, \lnot y), (\lnot x, y), (\lnot x, \lnot y)\rbrace$.

Binomial co-multiplication is equivalent to the logical \emph{or} operator
\cite{josang2005multiplication}. Given two opinions, again on distinct binary
frames, $\omega_x$ and $\omega_y$, the disjunctive binomial opinion
$\omega_{x \lor y} = \omega_x \sqcup \omega_y$ is computed by the following
function:

\begin{code}
(~*!) :: (ToBinomial op1, ToBinomial op2, Eq h, Ord b, Ord c)
         => SLExpr h a (op1 h b)
         -> SLExpr h a (op2 h c)
         -> SLExpr h a (Binomial h (F.Frame (b, c)))
opx ~*! opy = do
  opx' <- liftM toBinomial opx
  opy' <- liftM toBinomial opy
  require (bHolder opx' == bHolder opy') "opinions must have same holder"
  return $ cotimes' opx' opy'

cotimes' (Binomial bx dx ux ax hx xt xf) (Binomial by dy uy ay _ yt yf) =
  Binomial b' d' u' a' hx t f
  where
    b' = bx + by - bx * by
    d' = dx * dy + (ax * (1 - ay) * dx * uy + (1 - ax) * ay * ux * dy)
         / (ax + ay - ax * ay)
    u' = ux * uy + (ay * dx * uy + ax * ux * dy)
         / (ax + ay - ax * ay)
    a' = ax + ay - ax * ay

    t = F.fromList [(xt, yt), (xf, yt), (xt, yf)]
    f = F.singleton (xf, yf)
\end{code}

Binomial multiplication and co-multiplication are duals to one another
and satisfy De-Morgan's law:
$\omega_{x \land y} = \omega_{\overline{\overline{x} \lor \overline{y}}}$
and $\omega_{x \lor y} = \omega_{\overline{\overline{x} \land \overline{y}}}$,
but they do not distribute over one another \cite{josang2005multiplication}.
Josang and McAnally claim that binomial multiplication and
co-multiplication produce good approximations of the analytically correct
products and co-products of Beta probability density functions
\cite{josang2005multiplication}. Therefore, if one were to construct a
\emph{Beta} data type in Haskell representing a beta PDF and create an instance
of the \emph{ToBinomial} type class for it, one could use the above operators
to generate good approximations to the products and co-products of beta PDFs
with minimal effort.

We next discuss binomial division and co-division, which are the inverses
of binomial multiplication and co-multiplication. The binomial division
of an opinion $\omega_x$ by another opinion $\omega_y$ is denoted as
$\omega_{x \overline{\land} y} = \omega_x / \omega_y$
\cite{josang2005multiplication}, and is computed as follows:

\begin{code}
(/!) :: (ToBinomial op1, ToBinomial op2, Eq c)
        => SLExpr h a (op1 h (F.Frame (b, c)))
        -> SLExpr h a (op2 h b)
        -> SLExpr h a (Binomial h c)
opx /! opy = do
  opx' <- liftM toBinomial opx
  opy' <- liftM toBinomial opy
  require (lessBaseRate opx' opy') "ax must be less than ay"
  require (greaterDisbelief opx' opy') "dx must be greater than or equal to dy"
  require (bxConstraint opx' opy') "Division requirement not satisfied"
  require (uxConstraint opx' opy') "Division requirement not satisfied"
  return $ divide' opx' opy'
  where
    lessBaseRate x y     = bAtomicity x < bAtomicity y
    greaterDisbelief x y = bDisbelief x >= bDisbelief y

    bxConstraint x y = bx >= (ax * (1 - ay) * (1 - dx) * by) / ((1 - ax) * ay * (1 - dy))
      where
        (bx, dx, ux, ax) = (bBelief x, bDisbelief x, bUncertainty x, bAtomicity x)
        (by, dy, uy, ay) = (bBelief y, bDisbelief y, bUncertainty y, bAtomicity y)

    uxConstraint x y = ux >= ((1 - ay) * (1 - dx) * uy) / ((1 - ax) * (1 - dy))
      where
        (bx, dx, ux, ax) = (bBelief x, bDisbelief x, bUncertainty x, bAtomicity x)
        (by, dy, uy, ay) = (bBelief y, bDisbelief y, bUncertainty y, bAtomicity y)

divide' (Binomial bx dx ux ax hx xt xf) (Binomial by dy uy ay _ yt yf) =
  Binomial b' d' u' a' hx zt zf
  where
    b' = ay * (bx + ax * ux) / ((ay - ax) * (by + ay *uy))
         - ax * (1 - dx) / ((ay - ax) * (1 - dy))
    d' = (dx - dy) / (1 - dy)
    u' = ay * (1 - dx) / ((ay - ax) * (1 - dy))
         - ay * (bx + ax * ux) / ((ay - ax) * (bx + ay * uy))
    a' = ax / ay

    [(_, zt)] = F.toList xt
    zf = head . filter (/= zt) . map snd . F.toList $ xf
\end{code}

Lastly co-division, the inverse operation of co-multiplication \cite{josang2005multiplication},
is denoted as $\omega_{x \overline{\lor} y} = \omega_x \overline{\sqcup} \omega_y$ and is
computed as follows:

\begin{code}
(~/!) :: (ToBinomial op1, ToBinomial op2, Eq c)
         => SLExpr h a (op1 h (F.Frame (b, c)))
         -> SLExpr h a (op2 h b)
         -> SLExpr h a (Binomial h c)
opx ~/! opy = do
  opx' <- liftM toBinomial opx
  opy' <- liftM toBinomial opy
  require (greaterBaseRate opx' opy') "ax must be greater than ay"
  require (greaterBelief opx' opy') "bx must be greater than or equal to by"
  require (dxConstraint opx' opy') "Division requirement not satisfied"
  require (uxConstraint opx' opy') "Division requirement not satisfied"
  return $ codivide' opx' opy'
  where
    greaterBaseRate x y     = bAtomicity x > bAtomicity y
    greaterBelief x y = bBelief x >= bBelief y

    dxConstraint x y = dx >= (ay * (1 - ax) * (1 - bx) * dy) / ((1 - ay) * ax * (1 - by))
      where
        (bx, dx, ux, ax) = (bBelief x, bDisbelief x, bUncertainty x, bAtomicity x)
        (by, dy, uy, ay) = (bBelief y, bDisbelief y, bUncertainty y, bAtomicity y)

    uxConstraint x y = ux >= (ay * (1 - bx) * uy) / (ax * (1 - by))
      where
        (bx, dx, ux, ax) = (bBelief x, bDisbelief x, bUncertainty x, bAtomicity x)
        (by, dy, uy, ay) = (bBelief y, bDisbelief y, bUncertainty y, bAtomicity y)

codivide' (Binomial bx dx ux ax hx xt xf) (Binomial by dy uy ay _ yt yf) =
  Binomial b' d' u' a' hx zt zf
  where
    b' = (bx - by) / (1 - by)
    d' = ((1 - ay) * (dx + (1 - ax) * ux)
          / ((ax - ay) * (dy + (1 - ay) * uy)))
         - (1 - ax) * (1 - bx) / ((ax - ay) * (1 - by))
    u' = ((1 - ay) * (1 - bx) / ((ax - ay) * (1 - by)))
         - ((1 - ay) * (dx + (1 - ax) * ux)
            / ((ax - ay) * (dy + (1 - ay) * uy)))
    a' = (ax - ay) / (1 - ay)

    zt = head . filter (/= zf) . map snd . F.toList $ xt
    [(_, zf)] = F.toList xf
\end{code}

In this section we have introduced those binomial operators that have
analogs to logic and set theory. In the next section we discuss
the binomial operators for modeling \emph{trust transitivity}.


\subsubsection{Trust Transitivity Operators}

In this section we present the Subjective Logic operators for trust
transitivity. If two agents A and B exist such that agent A has an opinion
of agent B, and agent B has an opinion about some proposition X, then
A can form an opinion of X by \emph{discounting} B's opinion of x based on
A's opinion of B.

Subjective Logic offers three methods of discounting:
\emph{uncertainty favouring discounting}, \emph{opposite belief
favouring discounting}, and \emph{base rate sensitive discounting} \cite{josang2012trust}.
We begin by constructing a simple data type to represent the three kinds of discounting.

\begin{code}
data Favouring = Uncertainty | Opposite | BaseRateSensitive
\end{code}

By doing so, we are able to expose a single discounting function to
the user that selects the kind of discounting based on an input parameter of type
\emph{Favouring}:

\begin{code}
discount :: (ToBinomial op1, ToBinomial op2, Ord h, Ord b)
            => Favouring
            ->SLExpr h a (op1 h h)
            -> SLExpr h a (op2 h b)
            -> SLExpr h a (Binomial h b)
discount f opx opy = do
  opx' <- liftM toBinomial opx
  opy' <- liftM toBinomial opy
  return $ case f of
    Uncertainty       -> discount_u opx' opy'
    Opposite          -> discount_o opx' opy'
    BaseRateSensitive -> discount_b opx' opy'
\end{code}

Depending on the first parameter, the discount function dispatches to one
of three implementations: \emph{discount\_u}, \emph{discount\_o}, or \emph{discount\_b}.
Their definitions follow below.

\begin{code}
discount_u :: Binomial h h -> Binomial h a -> Binomial h a
discount_u (Binomial bb db ub ab hx _ _) (Binomial bx dx ux ax hy fx fy) =
  Binomial b' d' u' a' (Discount hx hy) fx fy
  where
    b' = bb * bx
    d' = bb * dx
    u' = db + ub + bb * ux
    a' = ax
\end{code}

\begin{code}
discount_o :: Binomial h h -> Binomial h a -> Binomial h a
discount_o (Binomial bb db ub ab hx _ _) (Binomial bx dx ux ax hy fx fy) =
  Binomial b' d' u' a' (Discount hx hy) fx fy
  where
    b' = bb * bx + db * dx
    d' = bb * dx + db * bx
    u' = ub + (bb + db) * ux
    a' = ax
\end{code}

\begin{code}
discount_b :: (Ord a, Ord h) => Binomial h h -> Binomial h a -> Binomial h a
discount_b op1@(Binomial bb db ub ab hx _ _) op2@(Binomial bx dx ux ax hy fx fy) =
  Binomial b' d' u' a' (Discount hx hy) fx fy
  where
    b' = expectation op1 * bx
    d' = expectation op1 * dx
    u' = 1 - expectation op1 * (bx + dx)
    a' = ax
\end{code}


In this section we have presented the operators of Subjective Logic for working with binomial opinions.
We first introduced the operators that have
analogs to the classical operators of logic and set theory, and then introduced
operators for modeling transitive trust networks. These operators are summarized
in Table \ref{tbl:binomial-operators}. In the next section we introduce the
operators of Subjective Logic for working with multinomial and hyper opinions.

\begin{table}
\begin{center}
\begin{tabular}{| l | l | l |}
  \hline
  Name & SL Notation & SLHS Notation\\
  \hline
  Addition          & $\omega_{X \cup Y} = \omega_X + \omega_Y$                  & $opx +! opy$  \\
  Subtraction       & $\omega_{X \setminus Y} = \omega_X - \omega_Y$             & $opx -! opy$  \\
  Negation          & $\omega_{\bar{x}} = \lnot \omega_x$                        & $negate opx$  \\
  Multiplication    & $\omega_{X \land Y} = \omega_X \cdot \omega_Y$             & $opx *! opy$  \\
  Co-multiplication & $\omega_{X \lor Y} = \omega_X \sqcup \omega_Y$             & $opx ~*! opy$ \\
  Division          & $\omega_{X \bar{\land} Y} = \omega_X / \omega_Y$           & $opx /! opy$  \\
  Co-division       & $\omega_{X \bar{\lor} Y} = \omega_X \bar{\sqcup} \omega_Y$ & $opx ~/! opy$ \\
  Discounting       & $\omega^{A:B}_x = \omega^A_B \otimes \omega^B_x$           & $discount\,t\,opa\,opb$ \\
  \hline
\end{tabular}
\end{center}

\caption{Summary of binomial operators}
\label{tbl:binomial-operators}
\end{table}




\subsection{Multinomial and Hyper Operators}

In this section we present the multinomial and hyper operators. We start with
multinomial multiplication and describe how it differs from binomial multiplication \cite{josang2005multiplication},
then we introduce the various operators for belief \emph{fusion} and \emph{unfusion}
\cite{josang2012interpretation, josang2010cumulative, josang2009fission, josang2009cumulative}.
We then introduce the \emph{deduction} and \emph{abduction} operators for reasoning \cite{josanginverting, josang2008conditional, josang2008abductive},
and lastly we introduce the \emph{belief constraint} operator \cite{josang2012dempster}.


\subsubsection{Multinomial Multiplication}

The multiplication of two multinomial opinions is a separate operator
than the product operator defined over binomial opinions. Whereas the
binomial product operator is equivalent to the logical \emph{and}
operator, multinomial multiplication constructs an opinion over a new
frame which is the cartesian product of the frames of the input opinions \cite{josang2005multiplication}.
In order to avoid symbolic naming conflicts, we have chosen to name the binomial
operator with the symbol $*!$, and we have used the name \emph{times} to
denote the multinomial operator.

\begin{code}
times :: (ToMultinomial op1, ToMultinomial op2, Eq h, Ord b, Ord c)
         => SLExpr h a (op1 h b) -> SLExpr h a (op2 h c)
         -> SLExpr h a (Multinomial h (b, c))
times opx opy = do
  opx' <- liftM toMultinomial opx
  opy' <- liftM toMultinomial opy
  return $ m_times' opx' opy'
\end{code}

\begin{code}
m_times' :: (Ord a, Ord b) => Multinomial h a -> Multinomial h b -> Multinomial h (a, b)
m_times' (Multinomial bx ux ax hx fx) (Multinomial by uy ay hy fy) =
  Multinomial b' u' a' (Product hx hy) (fx `F.cross` fy)
  where
    b' = V.fromList bxy
    u' = uxy
    a' = V.fromList axy

    bxy = [ ((x, y), f x y) | x <- xKeys, y <- yKeys ]
      where
        f x y = expect x y - (V.value ax x * V.value ay y * uxy)

    axy = [ ((x, y), f x y) | x <- xKeys, y <- yKeys ]
      where
        f x y = V.value ax x * V.value ay y

    uxy = minimum [ uxy' x y | x <- xKeys, y <- yKeys ]

    uxy' x y = (uIxy * expect x y) / (bIxy x y + V.value ax x * V.value ay y * uIxy)

    uIxy = uRxy + uCxy + uFxy
      where
        uRxy = sum [ ux * V.value by y | y <- yKeys ]
        uCxy = sum [ uy * V.value bx x | x <- xKeys ]
        uFxy = ux * uy

    bIxy x y = V.value bx x * V.value by y

    expect x y = (V.value bx x + V.value ax x * ux) * (V.value by y + V.value ay y * uy)

    xKeys = F.toList fx
    yKeys = F.toList fy
\end{code}


\subsubsection{Fusion, Unfusion, and Fission}

Hyper opinions can be fused together using two different operators:
\emph{cumulative fusion} and \emph{averaging fusion}. Each operator
should be used under different circumstances depending on the meaning
of the fused opinions \cite{josang2010cumulative, josang2012interpretation}.

\begin{code}
cFuse :: (ToHyper op1, ToHyper op2, Ord b)
         => SLExpr h a (op1 h b) -> SLExpr h a (op2 h b) -> SLExpr h a (Hyper h b)
cFuse opa opb = do
  opa' <- liftM toHyper opa
  opb' <- liftM toHyper opb
  return $ cFuse' opa' opb'
\end{code}

\begin{code}
cFuse' :: Ord a => Hyper h a -> Hyper h a -> Hyper h a
cFuse' (Hyper ba ua aa hx fx) (Hyper bb ub ab hy _)
  | ua /= 0 || ub /= 0 = Hyper b' u' a' (Fuse Cumulative hx hy) fx
  | otherwise          = Hyper b'' u'' a'' (Fuse Cumulative hx hy) fx
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

\begin{code}
aFuse :: (ToHyper op1, ToHyper op2, Ord a)
         => SLExpr h a (op1 h a) -> SLExpr h a (op2 h a) -> SLExpr h a (Hyper h a)
aFuse opa opb = do
  opa' <- liftM toHyper opa
  opb' <- liftM toHyper opb
  return $ aFuse' opa' opb'
\end{code}

\begin{code}
aFuse' :: Ord a => Hyper h a -> Hyper h a -> Hyper h a
aFuse' (Hyper ba ua aa hx fx) (Hyper bb ub ab hy _)
  | ua /= 0 || ub /= 0 = Hyper b' u' a' (Fuse Averaging hx hy) fx
  | otherwise          = Hyper b'' u'' a'' (Fuse Averaging hx hy) fx
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

Cumulative \emph{unfusion} is defined for multinomial opinions \cite{josang2009cumulative}.
It has yet to be generalized to hyper opinions. Given an opinion that represents
the result of cumulatively fusing together two opinions, and one of the
two original opinions, it is possible to extract the other original
opinion.

\begin{code}
cUnfuse :: (ToMultinomial op1, ToMultinomial op2, Ord a)
           => SLExpr h a (op1 h a) -> SLExpr h a (op2 h a)
           -> SLExpr h a (Multinomial h a)
cUnfuse opc opb = do
  opc' <- liftM toMultinomial opc
  opb' <- liftM toMultinomial opb
  return $ cUnfuse' opc' opb'
\end{code}

\begin{code}
cUnfuse' :: Ord a => Multinomial h a -> Multinomial h a -> Multinomial h a
cUnfuse' (Multinomial bc uc ac (Fuse Cumulative hx hy) fx) (Multinomial bb ub ab _ _)
  | uc /= 0 || ub /= 0 = Multinomial ba ua aa hx fx
  | otherwise          = Multinomial ba' ua' aa' hx fx
  where
    ba = V.mapWithKey belief bc
    ua = ub * uc / (ub - uc + ub * uc)
    aa = ac

    ba' = bb
    ua' = 0
    aa' = ac

    belief x b = (b * ub - V.value bb x  * uc) / (ub - uc + ub * uc)
\end{code}

Likewise, averaging unfusion is the inverse operation to averaging fusion
\cite{josang2009cumulative}.

\begin{code}
aUnfuse :: (ToMultinomial op1, ToMultinomial op2, Ord a)
           => SLExpr h a (op1 h a) -> SLExpr h a (op2 h a)
           -> SLExpr h a (Multinomial h a)
aUnfuse opc opb = do
  opc' <- liftM toMultinomial opc
  opb' <- liftM toMultinomial opb
  return $ aUnfuse' opc' opb'
\end{code}

\begin{code}
aUnfuse' :: Ord a => Multinomial h a -> Multinomial h a -> Multinomial h a
aUnfuse' (Multinomial bc uc ac (Fuse Averaging hx hy) fx) (Multinomial bb ub ab _ _)
  | uc /= 0 || ub /= 0 = Multinomial ba ua aa hx fx
  | otherwise          = Multinomial ba' ua' aa' hy fx
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
multinomial opinions based on some ratio $\phi$ \cite{josang2009fission}
We refer to this as the \emph{split} operator. Like unfusion, fission
has not yet been generalized to hyper opinions.

\begin{code}
cSplit :: (Ord a, ToMultinomial op) => Rational -> SLExpr h a (op h a)
          -> SLExpr h a (Multinomial h a , Multinomial h a)
cSplit phi op = do
  op' <- liftM toMultinomial op
  return $ cSplit' phi op'
\end{code}

\begin{code}
cSplit' :: Rational -> Multinomial h a -> (Multinomial h a, Multinomial h a)
cSplit' phi (Multinomial b u a (Fuse Cumulative h1 h2) fx) = (op1, op2)
  where
    op1 = Multinomial b1 u1 a h1 fx
    op2 = Multinomial b2 u2 a h2 fx

    b1 = V.map (\x -> phi * x / norm phi) b
    u1 = u / norm phi

    b2 = V.map (\x -> (1 - phi) * x / norm (1 - phi)) b
    u2 = u / norm (1 - phi)

    norm p = u + p * V.fold (+) 0 b
\end{code}








\subsubsection{Deduction and Abduction}

Deduction and abduction of multinomial opinions allows for one to do
conditional reasoning with Subjective Logic \cite{josanginverting, josang2008conditional, josang2008abductive}. We first introduce the
operator for performing deduction, which we call \emph{deduce}, and
then discuss the operator \emph{abduce} for performing abduction.

Because of the nature of these operators, the frames of discernment
which the opinions are defined over must satisfy two properties: they
must be \emph{bounded}, and the must be \emph{enumerable}. These
constraints on the type of frames allowed is expressed via the type
classes \emph{Bounded} and \emph{Enum}. Boundedness simply means that
there exists a least and greatest element, and enumerability means that
the values of the type must be enumerable.

We begin by introducing deduction.

\begin{code}
deduce :: (ToMultinomial op, Ord a, Bounded a, Enum a, Ord b, Bounded b, Enum b)
          => SLExpr h a (op h a)
          -> [(a, Multinomial h b)]
          -> SLExpr h a (Multinomial h b)
deduce opx ops = do
  opx' <- liftM toMultinomial opx
  return $ deduce' opx' ops
\end{code}

\begin{code}
deduce' :: forall a. forall b. forall h.
           (Ord a, Bounded a, Enum a, Ord b, Bounded b, Enum b)
           => Multinomial h a
           -> [(a, Multinomial h b)]
           -> Multinomial h b
deduce' opx@(Multinomial bx ux ax hx _) ops = Multinomial b' u' a' hx f
  where
\end{code}

\begin{code}
    expt y = sum . map f $ xs
      where
        f x = V.value ax x * V.value (expectation (findOpinion x)) y
\end{code}

\begin{code}
    expt' y = sum . map f $ xs
      where
        f x = V.value (expectation opx) x * V.value (expectation (findOpinion x)) y
\end{code}

\begin{code}
    tExpt y = (1 - V.value ay y) * byxs + (V.value ay y) * (byxr + uyxr)
      where
        (xr', xs') = dims y
        byxr       = V.value (mBelief xr') y
        uyxr       = mUncertainty xr'
        byxs       = V.value (mBelief xs') y
\end{code}

\begin{code}
    xs = [minBound .. maxBound] :: [a]
    ys = [minBound .. maxBound] :: [b]
\end{code}

\begin{code}
    ay = mBaseRate . snd . head $ ops
\end{code}

\begin{code}
    uYx x = maybe 1 mUncertainty . lookup x $ ops

    findOpinion x = case lookup x ops of
      Nothing -> Multinomial (V.fromList []) 1 ay hx f
      Just op -> op
\end{code}

\begin{code}
    f = mFrame . snd . head $ ops
\end{code}

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

\begin{code}
    triangleApexU y
      | expt y <= tExpt y = (expt y - byxs) / V.value ay y
      | otherwise         = (byxr + uyxr - expt y) / (1 - V.value ay y)
      where
        byxr = V.value (mBelief . fst . dims $ y) y
        uyxr = mUncertainty . fst . dims $ y
        byxs = V.value (mBelief . snd . dims $ y) y
\end{code}

\begin{code}
    intApexU = maximum . map triangleApexU $ ys
\end{code}

\begin{code}
    bComp y = expt y - V.value ay y * intApexU
\end{code}

\begin{code}
    adjustedU y | bComp y < 0 = expt y / V.value ay y
                | otherwise   = intApexU
\end{code}

\begin{code}
    apexU = minimum . map adjustedU $ ys
\end{code}

\begin{code}
    b' = V.fromList [ (y, expt' y - (V.value ay y) * u') | y <- ys ]
    u' = (apexU -) . sum . map (\x -> (apexU - uYx x) * V.value bx x) $ xs
    a' = ay
\end{code}


Subjective Logic abduction is a two step procedure. Given an opinion
over a frame X and a list of conditional opinions over X given Y, we
first must invert the conditionals into a list of conditional opinions
over Y given X, and then perform Subjective Logic deduction with the
new list and the opinion over X.

\begin{code}
abduce :: (ToMultinomial op, Ord a, Bounded a, Enum a, Ord b, Bounded b, Enum b)
          => SLExpr h a (op h a)
          -> [(b, Multinomial h a)]
          -> BaseRateVector b
          -> SLExpr h a (Multinomial h b)
abduce opx ops ay = do
  opx' <- liftM toMultinomial opx
  return $ abduce' opx' ops ay
\end{code}

\begin{code}
abduce' :: forall a. forall b. forall h.
           (Ord a, Bounded a, Enum a, Ord b, Bounded b, Enum b)
           => Multinomial h a
           -> [(b, Multinomial h a)]
           -> BaseRateVector b
           -> Multinomial h b
abduce' opx@(Multinomial bx ux ax hx fx) ops ay = deduce' opx ops'
  where
    ops' = map multinomial xs

    multinomial x = (x, Multinomial b' u' a' hx (F.fromList ys))
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

\begin{code}
    ax = mBaseRate . snd . head $ ops
\end{code}

\begin{code}
    xs = [minBound .. maxBound] :: [a]
    ys = [minBound .. maxBound] :: [b]
\end{code}

\begin{code}
    findOpinion y = case lookup y ops of
      Nothing -> Multinomial (V.fromList []) 1 ax hx (F.fromList xs)
      Just op -> op
\end{code}


\subsubsection{Belief Constraining}

The final operator we discuss is the \emph{belief constraint}
operator \cite{josang2012dempster}. This operator takes as input two objects that are convertible
to hyper opinions and returns a hyper opinion as output. This function
is equivalent in meaning to Dempster's rule of combination from Dempster-Shafer Theory \cite{josang2012dempster}.

\begin{code}
constraint :: (ToHyper op1, ToHyper op2, Ord b)
              => SLExpr h a (op1 h b)
              -> SLExpr h a (op2 h b)
              -> SLExpr h a (Hyper h b)
constraint op1 op2 = do
  op1' <- liftM toHyper op1
  op2' <- liftM toHyper op2
  return $ constraint' op1' op2'
\end{code}

\begin{code}
constraint' :: (Ord a) => Hyper h a -> Hyper h a -> Hyper h a
constraint' h1@(Hyper bA uA aA hx fx) h2@(Hyper bB uB aB hy _) =
  Hyper bAB uAB aAB (Constraint hx hy) fx
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
        matches = [(y, z) | y <- keys, z <- keys, F.intersection y z == x]

    conflict = sum . map combine $ matches
      where
        matches = [(y, z) | y <- keys, z <- keys, F.intersection y z == F.empty]

    combine (y, z) = V.value bA y * V.value bB z

    keys = F.toList $ F.reducedPowerSet fx
    keys' = nub (V.focals aA ++ V.focals aB)
\end{code}

The operators for multinomial and hyper opinions are summarized in table \ref{tbl:mh-operators}.

\begin{table}
\begin{center}
\begin{tabular}{| l | l | l |}
  \hline
  Name & SL Notation & SLHS Notation\\
  \hline
  Multiplication      & $\omega_{X \cup Y} = \omega_X + \omega_Y$                                                          & $opx\; `times`\; opy$  \\
  Deduction           & $\omega_{Y || X} = \omega_X \circledcirc \omega_{Y | X}$                                           & $deduce\; opx\; ops$  \\
  Abduction           & $\omega_{Y \overline{||} X} = \omega_X \overline{\circledcirc} \omega_{X | Y}$                     & $abduce\; opx\; opys\; a$  \\
  Cumulative Fusion   & $\omega^{A \diamondsuit B}_{X} = \omega^A_X \oplus \omega^B_X$                                     & $opx\; `cFuse`\; opy$  \\
  Cumulative Unfusion  & $\omega^{A \overline{\diamondsuit} B}_{X} = \omega^A_X \ominus \omega^B_X$                         & $opx\; `cUnfuse`\; opy$  \\
  Averaging  Fusion   & $\omega^{A \underline{\diamondsuit} B}_{X} = \omega^A_X \underline{\oplus} \omega^B_X$             & $opx\; `aFuse`\; opy$  \\
  Averaging  Unfusion  & $\omega^{A \overline{\underline{\diamondsuit}} B}_{X} = \omega^A_X \underline{\ominus} \omega^B_X$ & $opx\; `aUnfuse`\; opy$  \\
  Fission             & $\omega_{X \cup Y} = \omega_X + \omega_Y$                                                          & $split\; phi\; opx$  \\
  Belief Constraining & $\omega^{A \& B}_{X} = \omega^A_X \odot \omega^B_X$                                                & $opx\; `constraint`\; opy$  \\
  \hline
\end{tabular}
\end{center}

\caption{Summary of multinomial and hyper operators}
\label{tbl:mh-operators}
\end{table}







\end{document}
