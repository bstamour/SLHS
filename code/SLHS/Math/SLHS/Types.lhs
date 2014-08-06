
\documentclass[thesis.tex]{subfiles}

\begin{document}

\ignore{
\begin{code}
module Math.SLHS.Types where

import Data.Maybe
import Control.Applicative
import Control.Monad (ap)

import qualified Math.SLHS.Vector as V
import qualified Math.SLHS.Frame as F

import qualified Data.Map as M
\end{code}
}


\ignore{
\begin{code}
type BeliefVector a = V.Vector a
type BaseRateVector a = V.Vector a
\end{code}
}


\subsection{Belief Holders}

Subjective Logic opinions may include an optional belief holder. Belief holders play an
important role for operators such as \emph{transitive discounting}, where an agent's opinion
of an event is computed through said agent's opinion of a secondary agent, who holds an
opinion of the event in question. Other operators that utilize this meta-data are the various
belief fusion operators that are designed to merge opinions of events collected either from
different sensors (different belief holders), or from the same sensor but across different
periods of time.

We represent belief holders as a recursive data type in order to be able to capture complex
yet "imaginary" belief holders such as "the consensus of agents A, B and C."

\begin{code}
data Holder a = Holder a
              | Discount (Holder a) (Holder a)
              | Consensus (Holder a) (Holder a)
\end{code}


\subsection{Subjective Logic Values}

Values in SLHS are represented by a sum type:

\begin{code}
data SLVal a = SLVal a | Err String
\end{code}

Objects of type \emph{SLVal a} either contain a value of type \emph{a}, by virtue of the
\emph{SLVal} data constructor, or an error message, via the \emph{Err} data constructor. By
wrapping values in this intermediate type, we thus allow all operators in SLHS to return
either a value on success, or a detailed error message upon failure. This allows us to
report issues with Subjective Logic expressions that can only be detected at run-time.

Objects of type \emph{SLVal a} are also monads. The required type class instances for monad,
applicative, and functor are presented below:

\begin{code}
instance Monad SLVal where
  return = SLVal
  SLVal x >>= f = f x
  Err e   >>= _ = Err e

instance Applicative SLVal where
  pure = return
  (<*>) = ap

instance Functor SLVal where
  fmap = liftA
\end{code}

Furthermore, we provide a simple helper function for reporting errors. Since all functions
in Haskell must start with a lower case letter, we provide \emph{err}, just for syntactic
uniformity. Programs that use \emph{err} can also freely use the \emph{Err} data constructor
without any loss in functionality.

\begin{code}
err :: String -> SLVal a
err = Err
\end{code}




\subsection{Subjective Logic Expressions}

Expressions in Subjective Logic are represented as functions from some input
state to some output, such as an opinion, or a rational number.

\begin{code}
newtype SLExpr h a t = SLExpr (SLState h a -> (SLState h a, SLVal t))
\end{code}

The \emph{SLExpr} type is parameterized over three types:

\begin{itemize}
\item The type \emph{h} represents the type that all belief holders within the
expression must have. For example, if \emph{h} is instantiated to \emph{Int}, then
all belief holders must be represented by objects that inhabit the \emph{Int} type.

\item The type \emph{a} represents the types that make up the frames of discernment
within the expression. Any given expression can contain references to many frames,
but for simplicity of implementation, we enforce the rule that all frames must be
made up of elements of the same type. For example, all frames could be inhabited by
elements of type \emph{UserDefined}, where \emph{UserDefined} is a type that is
created by the user of the library.

\item The type \emph{t} represents the output type of the function. The output type
is, however, wrapped in the \emph{SLVal} type so that we can return meaningful
error messages to the users of the library.
\end{itemize}

All functions of type \emph{SLExpr} map objects of type \emph{SLState} to a pair:
the new state after evaluation of the expression, and the result of the expression.
\emph{SLState} is a simple aggregate type that allows us to thread the frames of
discernment and the belief mass assignments over those frames for each belief
holder.

\begin{code}
data SLState h a = SLState
                   { slsFrames     :: [F.Frame a]
                   , slsBeliefVecs :: M.Map h (BeliefVector a)
                   }
\end{code}

Finally, we provide a function \emph{run} that takes as input a Subjective Logic
expression and an initial state, and returns the updated state along with the value
computed by the expression.

\begin{code}
run :: SLExpr h a t -> SLState h a -> (SLState h a, SLVal t)
run (SLExpr f) st = f st
\end{code}

Lastly, objects of type \emph{SLExpr} form a \emph{monad}, and thus we can take
advantage of Haskell's support for programming with monads. We provide the definitions
for \emph{bind} and \emph{inject} below. Furthermore, since all monads are
applicative functors, and all applicative functors are functors, we provide those
definitions also. This allows the user of our library to program in a monadic,
applicative, or functorial style.

\begin{code}
instance Monad (SLExpr h a) where
  return x = SLExpr $ \st -> (st, SLVal x)
  ma >>= f = SLExpr $ \st -> let (st', a) = run ma st
                             in case a of
                               Err e   -> (st', Err e)
                               SLVal x -> run (f x) st'

instance Applicative (SLExpr h a) where
  pure = return
  (<*>) = ap

instance Functor (SLExpr h a) where
  fmap = liftA
\end{code}


\end{document}
