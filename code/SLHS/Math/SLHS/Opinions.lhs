\documentclass[thesis.tex]{subfiles}

\begin{document}



\section{Opinions}

In this section we will discuss the implementation of the most important
objects in SLHS from the user's standpoint: Subjective opinions. We start by
implementing binomial opinions, and then we present multinomial and hyper
opinions.


\ignore{
\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Math.SLHS.Opinions where

import Math.SLHS.Types
import qualified Math.SLHS.Vector as V
import qualified Math.SLHS.Frame as F

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Ratio
import Control.Arrow
import Control.Monad
\end{code}
}


\subsection{Binomial Opinions}

We represent binomial opnions by four rational numbers corresponding
to the belief, disbelief, uncertainty, and base rate of the opinion,
along with some additional meta-data: the belief holder and the frame
of discernment it is defined over. In code, the binomial opinion looks
like the following:

\begin{code}
data Binomial h a = Binomial { bBelief      :: Rational
                             , bDisbelief   :: Rational
                             , bUncertainty :: Rational
                             , bAtomicity   :: Rational
                             , bHolder      :: Holder h
                             , bFrame       :: BinomialFrame a
                             } deriving Show
\end{code}

Here we use Haskell's \emph{record syntax} to define the data constructor.
Haskell automatically creates the top-level functions \emph{bBelief},
\emph{bDisbelief}, \emph{bUncertainty}, \emph{bAtomicity}, and
\emph{bMetaData} that provide access to the "members" of the record.

Since binomial opinions can either be defined over a binary frame of
discernment, or a binary partitioning of a frame, we introduce a special
frame type just for binomials, called \emph{BinomialFrame}:

\begin{code}
data BinomialFrame a = Normal a a
                     | Split (F.Frame a) (F.Frame a)
                     deriving Show
\end{code}

The two data constructors, \emph{Normal} and \emph{Split} represent the two
possible kinds of frames that binomial opinions can be defined over.

Since certain binomial operators require the opinion to be defined over a
binary partitioning, we introduce a helper predicate that returns \emph{True}
when the binomial opinion is a partition, and \emph{False} otherwise:

\begin{code}
isPartitioned :: Binomial h a -> Bool
isPartitioned (Binomial _ _ _ _ _ (Normal _ _)) = True
isPartitioned _                                 = False
\end{code}

Lastly, we also introduce a special \emph{type class} called \emph{ToBinomial}
which allows us to define a range of types that can be converted to a
binomial opinion. An example of such a type could be a \emph{Beta
PDF}. We will re-use this strategy for implementing multinomial and
hyper opinions.

\begin{code}
class ToBinomial op where
  toBinomial :: op h a -> Binomial h a

instance ToBinomial Binomial where
  toBinomial = id
\end{code}


\subsection{Multinomial Opinions}

Multinomials are represented as records containing a \emph{BeliefVector} to represent the
amount of belief assigned to each element of the frame, a scalar rational number to
store the uncertainty mass, a \emph{BaseRateVector} which assigns each element in the frame
to a base rate, and a meta-data object.

\begin{code}
data Multinomial h a = Multinomial { mBelief      :: BeliefVector a
                                   , mUncertainty :: Rational
                                   , mBaseRate    :: BaseRateVector a
                                   , mHolder      :: Holder h
                                   , mFrame       :: F.Frame a
                                   } deriving Show
\end{code}

Just as in the case of binomials, we introduce a type class to represent types that can be
converted to multinomials. We provide the instance for multinomial opinions (the identity
function) as well as an instance for binomial opinions, since binomial opinions are a
special case of multinomial opinions.

\begin{code}
class ToMultinomial op where
  toMultinomial :: Ord a => op h a -> Multinomial h a

instance ToMultinomial Multinomial where
  toMultinomial = id

instance ToMultinomial Binomial where
  toMultinomial (Binomial b d u a h (Normal x y)) = Multinomial b' u a' h f
    where
      b' = V.fromList [ (x, b), (y, d) ]
      a' = V.fromList [ (x, a), (y, 1 - a) ]
      f  = F.fromList [x, y]

  toMultinomial (Binomial b d u a h (Split xs ys)) = Multinomial b' u a' h f
    where
      b' = V.fromList $
           [ (x, r) | x <- F.toList xs, let r = b / toRational (F.size xs) ]
           ++
           [ (y, r) | y <- F.toList ys, let r = d / toRational (F.size ys) ]
      a' = V.fromList $
           [ (x, r) | x <- F.toList xs, let r = a / toRational (F.size xs) ]
           ++
           [ (y, r) | y <- F.toList ys, let r = (1 - a) / toRational (F.size ys) ]
      f  = xs `F.union` ys
\end{code}

In the first case of the implementation of \emph{ToMultinomial Binomial}, where the
binomial opinion is defined over a frame of cardinality 2, we simply create the
relevant data structures for the multinomial opinion. In the second case, when the
binomial opinion is defined over a partition of a frame, we split the belief, disbelief,
and atomicity over the partitions to create the multinomial opinion. In a way, this
operation can be seen as a form of \emph{uncoarsening}.


\subsection{Hyper Opinions}

Hyper opinions share a similar structural layout to multinomial opinions
except the belief vector spans the reduced powerset of the frame, and is
thus represented as a \emph{BeliefVector} with sub-frames as the keys, instead of
elements of the frame.

\begin{code}
data Hyper h a = Hyper { hBelief      :: BeliefVector (F.Subframe a)
                       , hUncertainty :: Rational
                       , hBaseRate    :: BaseRateVector a
                       , hHolder      :: Holder h
                       , hFrame       :: F.Frame a
                       } deriving Show

class ToHyper op where
  toHyper :: Ord a => op h a -> Hyper h a

instance ToHyper Hyper where
  toHyper = id

instance ToHyper Multinomial where
  toHyper (Multinomial b u a h f) = Hyper b' u a h f
    where
      b' = V.fromList . map (first F.singleton) . V.toList $ b

instance ToHyper Binomial where
  toHyper = toHyper . toMultinomial
\end{code}


\subsection{The Opinion Type Class}

There are certain operations that are common amongst all opinions.
One example of such operation is the \emph{probability expectation}:
for binomials, the probability expectation is a simple scalar, whereas
for multinomial and hyper opinions the probability expectation is a
vector over the frame of discernment, and the reduced powerset of the
frame, respectively.

\begin{code}
class Opinion op h a where
  type ExpectationType op h a :: *

  expectation :: op h a -> ExpectationType op h a
  getFrame    :: op h a -> F.Frame a
\end{code}

In order to accomodate a function such as probability expectation that
returns a value of a different type depending on the type of the opinion,
we use an \emph{indexed type family}. For each opinion type, we associate
an "expectation type", which is the type one would obtain when querying the
probability expectation of the opinion. The instances for each of the three
opinion types follows.

\begin{code}
instance Ord a => Opinion Binomial h a where
  type ExpectationType Binomial h a = Rational

  expectation (Binomial b d u a _ _)          = b + a * u
  getFrame (Binomial _ _ _ _ _ (Normal x y))  = F.fromList [x, y]
  getFrame (Binomial _ _ _ _ _ (Split f1 f2)) = f1 `F.union` f2

instance Opinion Multinomial h a where
  type ExpectationType Multinomial h a = V.Vector a

  expectation _                      = undefined
  getFrame (Multinomial _ _ _ _ frm) = frm

instance Opinion Hyper h a where
  type ExpectationType Hyper h a = V.Vector a

  expectation _                = undefined
  getFrame (Hyper _ _ _ _ frm) = frm
\end{code}


\subsection{Belief Coarsening}
\label{sec:belief-coarsening}

Coarsening is an operation that takes a hyper opinion and converts it
into a binomial opinion. The inputs are an arbitrary hyper opinion and
a subset of the frame of discernment for which the hyper opinion is
defined over. Coarsening is a two-stage operation: First the frame of
discernment is partitioned into two sets: the subset given as input,
and everything else. These two subsets, taken together as a set, form
a new binary frame with which the new binomial opinion will be defined
over. Secondly, the belief masses associated with elements of the
powerset of the original frame via the hyper opinion input are split
up and assigned to the elements of the new frame. The resulting belief
mass assignment preserves additivity, and thus the new binomial
opinion is valid. The operation for coarsening is given below.

\begin{code}
coarsen :: (ToHyper op, Ord a) => SLExpr h a (op h a)
           -> F.Subframe a -> SLExpr h a (Binomial h (F.Subframe a))
coarsen op theta = liftM2 coarsen' op (return theta)
  where
    coarsen' op theta = Binomial b d u a undefined undefined
      where
        b = sumSnd . V.elemsWhere subset         $ belief
        d = sumSnd . V.elemsWhere emptyIntersect $ belief
        u = 1 - b - d
        a = sum . F.toList . F.map baseRate      $ theta

        belief   = hBelief . toHyper $ op
        baseRate = V.value (hBaseRate . toHyper $ op)

        sumSnd         = sum . map snd
        subset         = (`F.isSubsetOf` theta)
        emptyIntersect = F.isEmpty . (`F.intersection` theta)
\end{code}

As a convenience, we also offer a function to coarsen a hyper opinion,
not by an explicitly given subframe, but by those elements of the frame
that satisfy a given predicate. As an example, consider a frame of
discernment consisting of the natural numbers. Using the following
function, one can coarsen the frame into the binary frame
$\lbrace \mbox{evens}, \lnot \mbox{evens} \rbrace$.

\begin{code}
coarsenBy :: (ToHyper op, Ord a) => SLExpr h a (op h a)
             -> (a -> Bool) -> SLExpr h a (Binomial h (F.Frame a))
coarsenBy op pred = op >>= \op' ->
  let (theta, _) = F.partition pred . getFrame . toHyper $ op'
  in  coarsen op theta
\end{code}




\subsection{Accessing Opinion Expressions}

SLHS is built around combining together objects of type \emph{SLExpr}, which
are functions from some world state to some value. Since Subjective Logic operators
rely on opinions as inputs, we require a method of obtaining the opinions stored
in the state that is being threaded through behind the expressions. The following
functions do just that.

We start with fetching hyper opinions, as they are the most general. Given a belief
holder $h$ and an index $idx$ corresponding to the $idx$'th frame of discernment in
the state, \emph{getHyper} returns either a hyper opinion held by $h$ over opinion
$frames\lbrack idx \rbrack$, or a runtime error diagnostic.

\begin{code}
getHyper :: (Ord h, Ord a) => h -> Int -> SLExpr h a (Hyper h a)
getHyper holder idx = do
  frames <- liftM slsFrames getState
  vecs   <- liftM slsBeliefVecs getState
  rates  <- liftM slsBaseRateVecs getState
  if idx > length frames
    then err "getHyper: index out of range"
    else do let frm = frames !! idx
            case M.lookup frm vecs of
              Nothing -> err "getHyper: no mass assignments for that frame"
              Just m  -> do
                case M.lookup (Holder holder) m of
                  Nothing -> err "getHyper: no mass assignment for that holder"
                  Just m' -> do
                    return $ Hyper m' 0 (V.fromList []) (Holder holder) frm
\end{code}

While the above function looks fairly complicated, it simply unrwaps the relevant
state data from the \emph{SLExpr} monad, checks to see if the index is within the
bounds of the array of frames, and then looks to see if there are any mass assignments
for that particular frame. If there are mass assignments for that frame, then we
look up the particular mass assignment owned by the belief holder. If one exists, we
return it, else we return an error message.

Next we have a way of obtaining multinomial opinions. Since multinomial opinions
are a special case of hyper opinions, we first obtain the hyper opinion via a call to
\emph{getHyper}, and then check to see if we can safely convert that hyper opinion
into a multinomial opinion. If so, we return it, else we return an error message.

\begin{code}
getMultinomial :: (Ord h, Ord a) => h -> Int -> SLExpr h a (Multinomial h a)
getMultinomial holder f = do
  h <- getHyper holder f
  case maybeToMultinomial h of
    Nothing -> err "getMultinomial: not a multinomial opinion"
    Just m  -> return m
  where
    maybeToMultinomial (Hyper b u a h f) =
      let fs = V.focals b
      in if all (\f -> F.size f == 1) fs
         then let bv = V.toList b
                  bv' = map (\(a, r) -> ((F.toList a) !! 0, r)) bv
              in Just $ Multinomial (V.fromList bv') u a h f
         else Nothing
\end{code}

The same trick applies to obtaining binomial opinions. We first obtain the
relevant multinomial opinion and then see if we can safely convert it into
a binomial opinion. If so, great! Elsewise we return an error message to
the user.

\begin{code}
getBinomial :: (Ord h, Ord a) => h -> Int -> a -> SLExpr h a (Binomial h a)
getBinomial holder f x = do
  m <- getMultinomial holder f
  case maybeToBinomial x m of
    Nothing -> err "getBinomial: not a binomial opinion"
    Just b  -> return b
  where
    maybeToBinomial x (Multinomial b u a h f) = do
      guard (F.size f == 2)
      guard (x `F.member` f)
      let y = fst . head . V.elemsWhere (/= x) $ b
      let b' = V.value b x
      let d' = V.value b y
      let u' = 1 - b' - d'
      let a' = V.value a x
      return $ Binomial b' d' u' a' h (Normal x y)
\end{code}

In the above code for \emph{maybeToBinomial} we utilize the fact that the
\emph{Maybe} type is an instance of the type class \emph{MonadPlus}, which
gives us access to the \emph{guard} function. MonadPlus can be thought of
the set of types that are monads, but also have the properties of monoids
from algebra: a zero element (in the case of Maybe, the Nothing data
constructor), and a method of combining two MonadPlus objects together, which
in Haskell is called \emph{mplus}. Unfortunately the rules for identity and
associativity cannot be enforced in the language itself. An example instance of
MonadPlus for Maybe would be the following:

\begin{spec}
instance MonadPlus Maybe where
  mzero = Nothing
  Nothing  `mplus` x = x
  (Just x) `mplus` _ = x
\end{spec}



\end{document}
