\documentclass[thesis.tex]{subfiles}

\begin{document}

\ignore{
\begin{code}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Ex1 where

import Control.Applicative
import qualified Math.SLHS.Frame as F

import Math.SLHS
import Data.Ratio
\end{code}
}

\subsection{Going to the Movies}

The first situation is taken from the draft Subjective Logic
book\footnote{\url{http://folk.uio.no/josang/papers/subjective_logic.pdf}}
and it involves three friends trying to figure out
which movie they want to see. We start with defining the belief
holders as strings:

\begin{code}
holders = ["Alice", "Bob", "Clark"]
\end{code}

and then define the frame of discernment. Here we use a special type
to denote the three possible movie choices,
where \emph{BD} stands for \emph{Black Dust}, \emph{GM} stands for
\emph{Grey Matter}, and \emph{WP} stands for \emph{White Powder}:

\begin{code}
data Movie = BD | GM | WP deriving (Eq, Ord, Show, Bounded, Enum)
frame = [BD, GM, WP]
\end{code}

Now that we have the belief holders and the frame of discernment, we
can define the belief vectors. Since Subjective Logic expressions can involve
many frames, we define our data set to be a list of tuples: the first argument
is the frame which we will associate the data, and the second argument is
another list of tuples. This second list of tuples is comprised of the
belief owner, and a list of tuples containing subsets of the frame and
associated belief mass. The base rate data is defined similarly: for each
frame we associate a list of tuples: the first element being the belief
holder, and the second element being a list of elements of the frame paired
up with a-priori mass.

\begin{code}
vectors =
  [ (frame,
     [ ("Alice", [([BD], 99%100), ([GM], 1%100), ([WP], 0), ([GM, WP], 0)])
     , ("Bob",   [([BD], 0), ([GM], 1%100), ([WP], 99%100), ([GM, WP], 0)])
     , ("Clark", [([BD], 0), ([GM], 0), ([WP], 0), ([GM, WP], 1)])
     ])
  ]

baseRates =
  [ (frame,
     [ ("Alice", [(BD, 1%3), (GM, 1%3), (WP, 1%3)])
     , ("Bob",   [(BD, 1%3), (GM, 1%3), (WP, 1%3)])
     , ("Clark", [(BD, 1%3), (GM, 1%3), (WP, 1%3)])
     ])
  ]
\end{code}

In the above code, the \% operator constructs a rational number from the numerator
and denominator. Therefore, $1\%3$ results in the value $\frac{1}{3}$.

Once our data model has been defined, we can now perform calculations.
We start by constructing an initial state of the world, and then an
expression. The expression in this case is a simple application of the
belief constraint operator. We fetch the hyper opinions owned by the
three belief holders for frame 0 (the first and only frame in our list
of frames) and constrain the resulting hyper opinions.

\begin{code}
initial = makeState holders [frame] vectors baseRates

expr = getHyper "Alice" 0 `constraint`
       getHyper "Bob"   0 `constraint`
       getHyper "Clark" 0
\end{code}

Lastly, we can run the expression over the initial state of the world.
The resulting value is of type \emph{SLVal (Hyper String Movie)}, meaning
it is either a hyper opinion with belief owners modeled as strings and
frame elements being movies, or a run-time error diagnostic.

\begin{code}
result = initial >>= run' expr
\end{code}

When we run the command \emph{print result} we obtain the following:

\begin{spec}
Hyper:
  Holder: Constraint (Constraint (Holder "Alice") (Holder "Bob")) (Holder "Clark")
  Frame: {BD,GM,WP}
  Belief: <({BD},0 % 1),({BD,GM},0 % 1),({BD,WP},0 % 1),({GM},1 % 1),
           ({GM,WP},0 % 1),({WP},0 % 1)>
  Uncertainty: 0 % 1
  Base Rate: <(BD,1 % 3),(GM,1 % 3),(WP,1 % 3)>
\end{spec}

The resulting hyper opinion is held by the imaginary owner made up by
applying the \emph{Constraint} holder data constructor twice, defined over the
frame ${BD,GM,WP}$, and has 100\% belief allocated to the movie \emph{GM}, and
each movie has a base rate of $\frac{1}{3}$.

Note that the result of the calculation, that the three friends should see the movie
\emph{Grey Matter}, does not seem to be the intuitively correct answer. This can be
attributed to Clark's opinion, while it seemingly neglects to take into account that
neither Alice nor Bob seem to really want to see that movie. One method of fixing this
issue could be to introduce a \emph{weighted constraint} operator that places more
emphasis on different opinions. Since Alice and Bob seem much more certain regarding
which movie they want to see, perhaps more weight should be given to their opinions, and
less to Clark's.

\end{document}
