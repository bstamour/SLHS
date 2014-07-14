\documentclass[thesis.tex]{subfiles}


\begin{document}


\ignore{
\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}

module Math.SLHS.Operators.Multinomial (cSplit) where

import Math.SLHS.Opinions
import Math.SLHS.Types

import qualified Math.SLHS.Vector as V

import Data.Maybe
import Data.List
import Control.Applicative

import qualified Data.Map as M
\end{code}
}


\subsection{Multinomial Operators}


\subsubsection{Multinomial Multiplication}


\begin{code}
times' :: Multinomial h a -> Multinomial h a -> Multinomial h a
times' = undefined
\end{code}



\subsubsection{Multinomial Unfusion}


Multinomial unfusion is the inverse of the fusion of subjective
opinions. The reason why we present this operator first is that while
fusion has been generalized to hyper opinions, unfusion so far has
not.


\begin{code}
cUnfuse' :: Multinomial h a -> Multinomial h a -> Multinomial h a
cUnfuse' opC opb = undefined -- Compute opa
\end{code}


\subsubsection{Multinomial Fission}


Fission is the operation of splitting a multinomial opinion into two
multinomial opinions based on some ratio $\phi$. We refer to this as
the \emph{split} operator.


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

We will begin by introducing deduction. Since deduction is a fairly
complicated procedure in comparrison to the previously defined operators
we take advantage of \emph{local functions}: functions that are defined
only within the scope of other functions. As a simple example, consider
the following implementation of the \emph{factorial} function which is
\emph{tail-recursive}. Languages that support certain optimizations
can eliminate all tail-recursive function calls and thus generate faster
programs.

\begin{spec}
factorial :: Integer -> Integer
factorial n = work 1 n
  where
    work accum cur | cur == 0  = accum
                   | otherwise = work (accum * cur) (cur - 1)
\end{spec}

In the above function, the \emph{work} function is local to the
outer function, and thus is hidden from other functions. And now we
present the \emph{deduction} operator.


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
    xs = [minBound :: a .. maxBound :: a]
    ys = [minBound :: b .. maxBound :: b]
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
    intApexU = maximum [ triangleApexU y | y <- ys ]
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
    apexU = minimum [ adjustedU y | y <- ys ]
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
new list and the opinion over X.


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
    xs = [minBound :: a .. maxBound :: a]
    ys = [minBound :: b .. maxBound :: b]
\end{code}

\begin{code}
    -- TODO: Factor this out.
    findOpinion y = case lookup y ops of
      Nothing -> Multinomial (V.fromList []) 1 ax undefined
      Just op -> op
\end{code}


\end{document}
