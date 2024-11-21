{-# LANGUAGE DeriveAnyClass #-}

module Snake where

import Data.Patch
import Direction

data Snake = Snake
  { head :: Position,
    direction :: Direction,
    spine :: [(Direction, Int)]
  }
  deriving (Show)

snakeBody :: Snake -> [Position]
snakeBody (Snake pos _ spine) = concat $ scanl moveSegment [pos] spine
  where
    moveSegment points (direction, steps) =
      take steps $ tail $ iterate (applyAlways (opposite direction)) (last points)

data SnakeMove
  = Advance
  | Turn Direction

advanceHead :: Snake -> Snake
advanceHead (Snake pos direction spine) =
  Snake newPos direction updatedSpine
  where
    newPos = applyAlways direction pos
    updatedSpine = case spine of
      (dir, steps) : rest
        | dir == direction -> (dir, steps + 1) : rest
      _ -> (direction, 1) : spine

shrinkTail :: Snake -> Maybe Snake
shrinkTail snake@(Snake {spine}) =
  case spine of
    [] -> Nothing
    [(_, 1)] -> Nothing
    [(dir, steps)] -> Just $ snake {spine = [(dir, steps - 1)]}
    rest -> Just $ snake {spine = init rest <> lastSegment}
      where
        lastSegment = case last rest of
          (_, 1) -> []
          (dir, steps) -> [(dir, steps - 1)]
