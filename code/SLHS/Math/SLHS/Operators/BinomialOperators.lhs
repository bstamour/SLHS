\documentclass[thesis.tex]{subfiles}

\begin{document}

% TODO: Change add      -> sum
               subtract -> difference


\ignore{
\begin{code}
module Math.SLHS.Operators.BinomialOperators
       -- Logical operators.
       ( add
       , subtract
       , product
       , coproduct
       , quotient
       , coquotient
         -- discounting.
       , discount
       ) where

import Prelude hiding (subtract, product)
import Control.Monad (liftM2, join)
import Control.Applicative
import Math.SLHS.Core
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

The binomial operators in this section are ones that correspond to certain logical
or set theoretical operators when there is zero uncertainty. For example, the
\emph{product} operator below corresponds to the \emph{and}, or \emph{conjunction}
operator of binary logic when given dogmatic opinions as inputs.

We use the following notation for our operator implementations. For each operator,
we expose a function to the end user whose arguments are lifted into our expression
monad. This way, sub expressions can be used as inputs to the operators with zero
additional work from the end user's perspective. For each operator \emph{foo}, we
have a matching, internal operator \emph{foo'} that does the actual computation. The
internal operators take their parameters as non-monadic types, which makes them easier
to inspect, and returns a value wrapped in the monad. Thus the external function
\emph{foo}'s job is simply to unwrap it's parameters, compute the result, and then
flatten the monadic structure via the monadic function \emph{join}.

We begin with the internal function definitions:

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


And now we lift the operators into the expression monad.


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

In terms of our implementation, we take the same notion as before: we introduce an internal
function \emph{discount'} and expose the function \emph{discount} to the end users of our
library. \emph{discount'} is a simple function that takes in a favouring as it's first
argument and dispatches the computation to one of three helper functions, for each
favouring type respectively.

\begin{code}
discount' :: Favouring -> Binomial a -> Binomial a -> SLExpr a (Binomial a)
discount' Uncertainty       = discount_u
discount' Opposite          = discount_o
discount' BaseRateSensitive = discount_b
\end{code}

Next we have the three implementations of discounting. First, uncertainty favouring
discounting:

\begin{code}
discount_u :: Binomial a -> Binomial a -> SLExpr a (Binomial a)
discount_u (Binomial bb db ub ab _ _) (Binomial bx dx ux ax _ _) =
  return $ Binomial b' d' u' a' undefined undefined
  where
    b' = bb * bx
    d' = bb * dx
    u' = db + ub + bb * ux
    a' = ax
\end{code}

Next, opposite belief favouring discounting:

\begin{code}
discount_o :: Binomial a -> Binomial a -> SLExpr a (Binomial a)
discount_o (Binomial bb db ub ab _ _) (Binomial bx dx ux ax _ _) =
  return $ Binomial b' d' u' a' undefined undefined
  where
    b' = bb * bx + db * dx
    d' = bb * dx + db * bx
    u' = ub + (bb + db) * ux
    a' = ax
\end{code}

And lastly, base rate sensitive discounting:

\begin{code}
discount_b :: Binomial a -> Binomial a -> SLExpr a (Binomial a)
discount_b op1@(Binomial bb db ub ab _ _) op2@(Binomial bx dx ux ax _ _) =
  return $ Binomial b' d' u' a' undefined undefined
  where
    b' = expectation op1 * bx
    d' = expectation op1 * dx
    u' = 1 - expectation op1 * (bx + dx)
    a' = ax
\end{code}

To finish things up, we wrap the \emph{discount'} function into the monad for
end-user consumption:

\begin{code}
discount :: (ToBinomial op1, ToBinomial op2)
            => Favouring
            ->SLExpr a (op1 a) -> SLExpr a (op2 a) -> SLExpr a (Binomial a)
discount f opx opy = join $ discount' f <$>
                     (fmap toBinomial opx) <*> (fmap toBinomial opy)
\end{code}



\subsubsection{Reasoning Operators}

TODO: Implement deduction and abduction.


\end{document}
