

\begin{code}
module Math.SLHS.Frame where

import qualified Data.Set as S

newtype Frame a = Frame { unFrame :: S.Set a }

\end{code}
