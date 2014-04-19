\documentclass[a4paper]{article}

\usepackage{verbatim}
%\newenvironment{code}{\footnotesize\verbatim}{\endverbatim\normalsize}


\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
    {\lstset{}%
      \csname lst@SetFirstLabel\endcsname}
    {\csname lst@SaveFirstLabel\endcsname}
    \lstset{
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
               {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
               {\\\\}{{\char`\\\char`\\}}1
               {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
               {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
               {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
               {>>}{{>>}}2 {>>=}{{>>=}}2
               {|}{{$\mid$}}1               
    }



\title{A Purely Functional Subjective Logic Combinator Library}

\author{Bryan St. Amour}


\begin{document}
\maketitle



\begin{code}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}


module Foo where


import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Monad.Trans.State
import Control.Applicative

import qualified Data.Map as M
import qualified Data.Set as S


data BinomialOpinion a o =
  BinOp { b_belief      :: Rational
        , b_disbelief   :: Rational
        , b_uncertainty :: Rational
        , b_baseRate    :: Rational
        , b_x           :: a
        , b_not_x       :: a
        , b_owner       :: Owner o
        } deriving Show


class ToBinomial x where
  toBinomial :: x a o -> BinomialOpinion a o


instance ToBinomial BinomialOpinion where
  toBinomial = id


data MultinomialOpinion a o =
  MultiOp { m_belief      :: M.Map a Rational
          , m_uncertainty :: Rational
          , m_baseRate    :: M.Map a Rational
          , m_frame       :: S.Set a
          , m_owner       :: Owner o
          } deriving Show


class ToMultinomial x where
  toMultinomial :: Ord a => x a o -> MultinomialOpinion a o


instance ToMultinomial BinomialOpinion where
  toMultinomial (BinOp b d u a x y o) = MultiOp bel u br f o
    where
      bel = (M.fromList [(x, b), (y, d)])
      br  = (M.fromList [(x, a), (y, 1 - a)])
      f   = S.fromList [x, y]


instance ToMultinomial MultinomialOpinion where
  toMultinomial = id


data HyperOpinion a o =
  HypOp { h_belief      :: M.Map [a] Rational
        , h_uncertainty :: Rational
        , h_baseRate    :: M.Map a Rational
        , h_frame       :: S.Set a
        , h_owner       :: Owner o
        } deriving Show


class ToHyper x where
  toHyper :: Ord a => x a o -> HyperOpinion a o


instance ToHyper BinomialOpinion where
  toHyper = toHyper . toMultinomial


instance ToHyper MultinomialOpinion where
  toHyper (MultiOp b u a f o) = HypOp b' u a f o
    where
      b' = M.mapKeys pure b


instance ToHyper HyperOpinion where
  toHyper = id


bin_add' :: BinomialOpinion a o -> BinomialOpinion a o -> BinomialOpinion a o
bin_add' = undefined


bin_subtract' :: BinomialOpinion a o -> BinomialOpinion a o -> BinomialOpinion a o
bin_subtract' = undefined


bin_multiply' :: BinomialOpinion a o -> BinomialOpinion a o -> BinomialOpinion a o
bin_multiply' = undefined


bin_comultiply' :: BinomialOpinion a o -> BinomialOpinion a o -> BinomialOpinion a o
bin_comultiply' = undefined


bin_divide' :: BinomialOpinion a o -> BinomialOpinion a o -> BinomialOpinion a o
bin_divide' = undefined


bin_codivide' :: BinomialOpinion a o -> BinomialOpinion a o -> BinomialOpinion a o
bin_codivide' = undefined


type SLMonad a b = StateT (SLStateData a) (Either String) b


bin_add :: ToBinomial op
           => SLMonad a (op a o)
           -> SLMonad a (op a o)
           -> SLMonad a (BinomialOpinion a o)
bin_add op1 op2 = do
  op1' <- op1
  op2' <- op2
  return $ bin_add' (toBinomial op1') (toBinomial op2')


bin_subtract :: ToBinomial op
                => SLMonad a (op a o)
                -> SLMonad a (op a o)
                -> SLMonad a (BinomialOpinion a o)
bin_subtract op1 op2 = do
  op1' <- op1
  op2' <- op2
  return $ bin_subtract' (toBinomial op1') (toBinomial op2')


bin_multiply :: ToBinomial op
                => SLMonad a (op a o)
                -> SLMonad a (op a o)
                -> SLMonad a (BinomialOpinion a o)
bin_multiply op1 op2 = do
  op1' <- op1
  op2' <- op2
  return $ bin_multiply' (toBinomial op1') (toBinomial op2')


bin_divide :: ToBinomial op
              => SLMonad a (op a o)
              -> SLMonad a (op a o)
              -> SLMonad a (BinomialOpinion a o)
bin_divide op1 op2 = err "Not yet implemented."


data SLStateDatum a =
  SLStateDatum { slsFrame    :: S.Set a
               , slsBaseRate :: M.Map a Rational
               }


newtype SLStateData a = SLStateData [SLStateDatum a]


err :: String -> SLMonad a b
err = StateT . pure . Left


data Owner a where
  NoOwner   :: Owner a
  Owner     :: a -> Owner a
  Consensus :: Owner a -> Owner a -> Owner a
  Discount  :: Owner a -> Owner a -> Owner a
  AFuse     :: Owner a -> Owner a -> Owner a
  CFuse     :: Owner a -> Owner a -> Owner a


deriving instance Show a => Show (Owner a)
\end{code}





\end{document}

