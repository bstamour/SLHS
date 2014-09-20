
\documentclass[thesis.tex]{subfiles}

\begin{document}

\ignore{
\begin{code}



module Math.SLHS.Types where

import Data.Maybe
import Control.Applicative
import Control.Monad (ap)
import Control.Arrow
import Control.Monad

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
important role for operators such as \emph{transitive discounting} \cite{josang2006trust}, where an agent's opinion
of an event is computed through its opinion of a secondary agent, who holds an
opinion of the event in question. Other operators that utilize this information are the various
belief fusion operators that are designed to merge opinions of events collected either from
different sensors, or from the same sensor but across different
periods of time.

We represent belief holders as a recursive data type in order to be able to capture complex
yet "imaginary" belief holders such as "the consensus of agents A, B and C."

\begin{code}
data Holder a = None
              | Holder a
              | Product (Holder a) (Holder a)
              | Discount (Holder a) (Holder a)
              | Fuse FusionType (Holder a) (Holder a)
              | Constraint (Holder a) (Holder a)
              deriving (Eq, Ord, Show)
\end{code}

Since there are different ways in which two belief holders can be fused into an imaginary holder,
the \emph{Fuse} data constructor above takes in an argument of type \emph{FusionType}, which is
shown below.

\begin{code}
data FusionType = Cumulative
                | Averaging
                deriving (Eq, Ord, Show)
\end{code}



\subsection{Subjective Logic Values}

Values in SLHS are represented by the following type:

\begin{code}
data SLVal a = SLVal a
             | Err String
             deriving Show
\end{code}

Objects of type \emph{SLVal a} either contain a value of type \emph{a}, via the
\emph{SLVal} data constructor, or an error message, via the \emph{Err} data constructor. By
wrapping values in this intermediate type, we thus allow all operators in SLHS to return
either a value on success, or a detailed error message upon failure. This allows us to
report issues with Subjective Logic expressions that can only be detected at run-time.

Objects of type \emph{SLVal a} are also monads. The required type class instance is

\begin{code}
instance Monad SLVal where
  return = SLVal
  SLVal x >>= f = f x
  Err e   >>= _ = Err e
\end{code}

\ignore{
\begin{code}
instance Applicative SLVal where
  pure = return
  (<*>) = ap

instance Functor SLVal where
  fmap = liftA
\end{code}
}






\subsection{Subjective Logic Expressions}

Expressions in Subjective Logic are represented as functions from some input
state to some output, such as an opinion, or a rational number.

\begin{code}
newtype SLExpr h a t = SLExpr (SLState h a -> SLVal (SLState h a, t))
\end{code}

The \emph{SLExpr} type is parametrized over three types:

\begin{itemize}
\item The type \emph{h} represents the type that all belief holders within the
Subjective Logic expression must have. For example, if \emph{h} is instantiated to \emph{Int}, then
all belief holders must be represented by objects that inhabit the \emph{Int} type.

\item The type \emph{a} represents the types that make up the frames of discernment
within the expression. Any given Subjective Logic expression can contain references to many frames,
but for simplicity of implementation, we enforce the rule that all frames must be
made up of elements of the same type. For example, all frames could be inhabited by
elements of type \emph{UserDefined}, where \emph{UserDefined} is a type that is
created by the user of the library.

\item The type \emph{t} represents the output type of the function. The output type
is, however, wrapped in the \emph{SLVal} type so that we can return meaningful
error messages to the users of the library. We also include the updated state in the
output.
\end{itemize}

All functions of type \emph{SLExpr} map objects of type \emph{SLState} to a pair:
the new state after evaluation of the expression, and the result of the expression.
\emph{SLState} is a simple aggregate type that allows us to thread the frames of
discernment and the belief mass assignments over those frames for each belief
holder.

\begin{code}
data SLState h a =
  SLState
  { slsFrames       :: [F.Frame a]
  , slsBeliefVecs   :: M.Map (F.Frame a) (M.Map (Holder h) (BeliefVector (F.Frame a)))
  , slsBaseRateVecs :: M.Map (F.Frame a) (M.Map (Holder h) (BaseRateVector a))
  } deriving (Show)
\end{code}





\ignore{
\begin{code}
makeState :: (Ord a, Ord h)
             => [h]
             -> [[a]]
             -> [([a], [(h, [([a], Rational)])])]
             -> [([a], [(h, [(a, Rational)])])]
             -> SLVal (SLState h a)
makeState holders frames belVecs aVecs = return $ SLState frames' belVecs'' aVecs''
  where
    frames' = map F.fromList frames

    belVecs'  = M.fromList . map (first F.fromList) $ belVecs
    belVecs'' = M.map (M.fromList . map (Holder *** makeBVec)) belVecs'

    aVecs'  = M.fromList $ map (first F.fromList) aVecs
    aVecs'' = M.map (M.fromList . map (Holder *** V.fromList)) aVecs'

    makeBVec = V.fromList . map (first F.fromList)
\end{code}
}








\ignore{
\begin{code}
getState :: SLExpr h a (SLState h a)
getState = SLExpr $ \s -> return (s, s)
\end{code}
}



\ignore{
\begin{code}
require :: Bool -> String -> SLExpr h a ()
require True _  = return ()
require False e = err e
\end{code}
}





We provide a function \emph{run} that takes as input a Subjective Logic
expression and an initial state, and returns the updated state along with the value
computed by the expression.

\begin{code}
run :: SLExpr h a t -> SLState h a -> SLVal (SLState h a, t)
run (SLExpr f) st = f st
\end{code}

If the user does not care about the final state of the computation and only wants
to see the final value, we provide the function \emph{run'}:

\begin{code}
run' :: SLExpr h a t -> SLState h a -> SLVal t
run' (SLExpr f) st = liftM snd $ f st
\end{code}

Lastly, objects of type \emph{SLExpr} form a \emph{monad}, and thus we can take
advantage of Haskell's support for programming with monads. We provide the definitions
for \emph{bind} and \emph{inject} below. Furthermore, since all monads are
applicative functors, and all applicative functors are functors, we provide those
definitions also. This allows the user of our library to program in a monadic,
applicative, or functorial style.

\begin{code}
instance Monad (SLExpr h a) where
  return x = SLExpr $ \st -> return (st, x)

  ma >>= f = SLExpr $ \st -> case (run ma st) of
    Err e          -> Err e
    SLVal (st', a) -> let mb = f a in case run mb st' of
      Err e   -> Err e
      SLVal r -> SLVal r

instance Applicative (SLExpr h a) where
  pure = return
  (<*>) = ap

instance Functor (SLExpr h a) where
  fmap = liftA
\end{code}



\ignore{
\begin{code}
err :: String -> SLExpr h a t
err e = SLExpr $ \_ -> Err e
\end{code}
}


\end{document}
