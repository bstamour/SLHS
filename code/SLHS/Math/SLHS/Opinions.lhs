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

data BinomialFrame a = Normal a a
                     | Split (F.Frame a) (F.Frame a)
                     deriving Show

isPartitioned :: Binomial h a -> Bool
isPartitioned (Binomial _ _ _ _ _ (Normal _ _)) = True
isPartitioned _                                 = False
\end{code}







\ignore{
\begin{code}
data Partition h a = Partition { pBelief      :: Rational
                               , pDisbelief   :: Rational
                               , pUncertainty :: Rational
                               , pAtomicity   :: Rational
                               , pHolder      :: Holder h
                               , pFrame       :: (F.Subframe a, F.Subframe a)
                               } deriving Show
\end{code}
}




Here we use Haskell's \emph{record syntax} to define the data constructor.
Haskell automatically creates the top-level functions \emph{bBelief},
\emph{bDisbelief}, \emph{bUncertainty}, \emph{bAtomicity}, and
\emph{bMetaData} that provide access to the "members" of the record.

We also introduce a special \emph{type class} called \emph{ToBinomial}
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
  toMultinomial :: op h a -> Multinomial h a

instance ToMultinomial Multinomial where
  toMultinomial = id

instance ToMultinomial Binomial where
  toMultinomial = undefined
\end{code}


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
  toHyper :: op h a -> Hyper h a

instance ToHyper Hyper where
  toHyper = id

instance ToHyper Multinomial where
  toHyper = undefined

instance ToHyper Binomial where
  toHyper = undefined
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
  type ExpectationType Binomial h a    = Rational
  expectation (Binomial b d u a _ _)   = b + a * u
  getFrame (Binomial _ _ _ _ _ (Normal x y)) = F.fromList [x, y]
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




\ignore{
\begin{code}
instance Ord a => Opinion Partition h a where
  type ExpectationType Partition h a    = Rational
  expectation (Partition b d u a _ _)   = b + a * u
  getFrame (Partition _ _ _ _ _ (x, y)) = x `F.union` x
\end{code}
}






\ignore{
\begin{code}
getBinomial :: (Ord h, Ord a) => h -> Int -> a -> SLExpr h a (Binomial h a)
getBinomial holder f x = do
  m <- getMultinomial holder f
  case maybeToBinomial x m of
    Nothing -> err "getBinomial: not a binomial opinion"
    Just b  -> return b

getMultinomial :: (Ord h, Ord a) => h -> Int -> SLExpr h a (Multinomial h a)
getMultinomial holder f = do
  h <- getHyper holder f
  case maybeToMultinomial h of
    Nothing -> err "getMultinomial: not a multinomial opinion"
    Just m  -> return m

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

maybeToMultinomial :: Ord a => Hyper h a -> Maybe (Multinomial h a)
maybeToMultinomial (Hyper b u a h f) =
  let fs = V.focals b
  in if all (\f -> F.size f == 1) fs
     then let bv = V.toList b
              bv' = map (\(a, r) -> ((F.toList a) !! 0, r)) bv
          in Just $ Multinomial (V.fromList bv') u a h f
     else Nothing

maybeToBinomial :: Ord a => a -> Multinomial h a -> Maybe (Binomial h a)
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
}




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


\end{document}
