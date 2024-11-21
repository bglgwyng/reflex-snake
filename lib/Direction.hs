module Direction where

import Data.Patch

type Position = (Int, Int)

data Direction = Up | Down | Left | Right deriving (Show, Read, Eq, Ord)

instance Patch Direction where
  type PatchTarget Direction = Position
  apply dir (x, y) = pure (x + dx, y + dy)
    where
      (dx, dy) = delta dir

opposite :: Direction -> Direction
opposite = \case
  Up -> Down
  Down -> Up
  Direction.Left -> Direction.Right
  Direction.Right -> Direction.Left

delta :: Direction -> (Int, Int)
delta = \case
  Up -> (-1, 0)
  Down -> (1, 0)
  Direction.Left -> (0, -1)
  Direction.Right -> (0, 1)