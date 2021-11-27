module Aoc.Sort where

import Control.Monad
import Control.Monad.Primitive
import qualified Data.Vector.Algorithms.Intro as I
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as M

-- | In-place sort for mutable vectors, either in IO or ST
sort :: (Ord e, PrimMonad m, M.MVector v e) => v (PrimState m) e -> m ()
sort = I.sort

-- | Create a mutable copy of an immutable vector
mutInit ::
  (Ord e, PrimMonad m, V.Vector v e, M.MVector mv e) =>
  v e ->
  m (mv (PrimState m) e)
mutInit iv = do
  let len = V.length iv
  mv <- M.unsafeNew len
  forM_ [0 .. len -1] $ \i -> M.write mv i (iv V.! i)
  pure mv

-- | Sort an immutable vector.
sort' :: (Ord e, PrimMonad m, V.Vector v e) => v e -> m (v e)
sort' iv = do
  mv <- mutInit iv
  I.sort mv
  V.unsafeFreeze mv
