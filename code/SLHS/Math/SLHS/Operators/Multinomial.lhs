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


\begin{code}
cUnfuse' :: Multinomial h a -> Multinomial h a -> Multinomial h a
cUnfuse' opC opb = undefined -- Compute opa
\end{code}





\subsubsection{Multinomial Fission}


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
    -- Step 0:
    -- Some helper functions.

    -- The expectation of y given a vacuous expectation on X.
    expt y = sum . map f $ xs
      where
        f x = V.value ax x * V.value (expectation (findOpinion x)) y

    -- The expectation of y given X.
    expt' y = sum . map f $ xs
      where
        f x = V.value (expectation opx) x * V.value (expectation (findOpinion x)) y

    -- The expectation of y given the theoretical maximum uncertainty.
    tExpt y = (1 - V.value ay y) * byxs + (V.value ay y) * (byxr + uyxr)
      where
        (xr', xs') = dims y
        byxr       = V.value (mBelief xr') y
        uyxr       = mUncertainty xr'
        byxs       = V.value (mBelief xs') y

    -- All values of frames X and Y. We need to enumerate them in their entirity.
    xs = [minBound :: a .. maxBound :: a]
    ys = [minBound :: b .. maxBound :: b]

    -- All of the base rate vectors must be the same, so take the first one.
    ay = mBaseRate . snd . head $ ops

    -- Helper function.
    uYx x = maybe 1 mUncertainty . lookup x $ ops

    -- Helper function.
    findOpinion x = case lookup x ops of
      Nothing -> Multinomial (V.fromList []) 1 ay undefined
      Just op -> op

    -- Step 1:
    -- For each y, we compute the pair (xr, xs) that yields the theoretical
    -- maximum uncertainty.
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

    -- Step 2:
    -- Compute the triangle apex uncertainty for each y.
    triangleApexU y
      | expt y <= tExpt y = (expt y - byxs) / V.value ay y
      | otherwise         = (byxr + uyxr - expt y) / (1 - V.value ay y)
      where
        byxr = V.value (mBelief . fst . dims $ y) y
        uyxr = mUncertainty . fst . dims $ y
        byxs = V.value (mBelief . snd . dims $ y) y

    -- Intermediate sub-simplex apex uncertainty.
    intApexU = maximum [ triangleApexU y | y <- ys ]

    -- Step 3:
    -- Compute the intermediate belief components.
    bComp y = expt y - V.value ay y * intApexU

    -- Compute the adjusted apex uncertainty.
    adjustedU y | bComp y < 0 = expt y / V.value ay y
                | otherwise   = intApexU

    -- Finally compute the sub-simplex apex uncertainty.
    apexU = minimum [ adjustedU y | y <- ys ]

    -- Step 4:
    -- The final values we expect.
    b' = V.fromList [ (y, expt' y - (V.value ay y) * u') | y <- ys ]
    u' = (apexU -) . sum . map (\x -> (apexU - uYx x) * V.value bx x) $ xs
    a' = ay
\end{code}







\begin{code}
abduce :: (ToMultinomial op, Ord a, Bounded a, Enum a, Ord b, Bounded b, Enum b)
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
    ops' = map multinomial xs

    multinomial x = (x, Multinomial b' u' a' undefined)
      where
        b'  = V.fromList bs
        u'  = uT x
        a'  = ay
        bs  = [ (y, f y) | y <- ys ]
        f y = expt y x - V.value ay y * uT x

    expt y x = numer / denom
      where
        numer = V.value ay y * V.value (expectation (findOpinion y)) x
        denom = sum . map f $ ys
        f y   = V.value ay y  * V.value (expectation (findOpinion y)) x

    uT x = minimum [ f y | y <- ys ]
      where
        f y = expt y x / V.value ay y

    -- TODO: Confirm this.
    ax = mBaseRate . snd . head $ ops

    xs = [minBound :: a .. maxBound :: a]
    ys = [minBound :: b .. maxBound :: b]

    -- TODO: Factor this out.
    findOpinion y = case lookup y ops of
      Nothing -> Multinomial (V.fromList []) 1 ax undefined
      Just op -> op
\end{code}





\end{document}
