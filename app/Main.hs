{-# OPTIONS_GHC -Wno-unused-do-bind #-}

import Control.Arrow hiding (app)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Bool (bool)
import Data.Function
import Data.Functor
import Data.Maybe hiding (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Traversable
import Direction
import Graphics.Vty qualified as V
import Graphics.Vty.Attributes
import Reflex
import Reflex.Network
import Reflex.Utils
import Reflex.Vty
import Reflex.Vty.Utils
import Sampler
import Snake
import System.Random (newStdGen)

main :: IO ()
main =
  mainWidget $ withCtrlC $ initManager_ do
    app

app :: (VtyExample t m) => m ()
app = mdo
  dPlaying <- toggle False eToggle

  eToggle <-
    networkView
      ( dPlaying
          `ffor` bool
            ( grout (fixed (pure width)) . row $
                grout (fixed (pure height)) . col $
                  center $
                    grout (fixed 3) $
                      textButtonStatic def $
                        centerText "Play" ' ' (width - 2)
            )
            snakeGame
      )
      >>= switchHold never

  pure ()

snakeGame :: (VtyExample t m) => m (Event t ())
snakeGame = do
  foodGen <- liftIO newStdGen
  mdo
    eKeyInput <- input
    eTick <-
      switchDyn
        <$> networkReset
          (tickLossyFromPostBuildTime 0.5 `ffor` void)
          (void eMoveSnake)

    dSnake <- holdDyn initialSnake eMoveSnake
    bFood <-
      holdSampler
        ( do
            body <- Set.fromList . snakeBody <$> current dSnake
            let freeSpaces = [(y, x) | y <- [0 .. height - 1], x <- [0 .. width - 1], (y, x) `Set.notMember` body]
            pure (sampleFromList freeSpaces)
        )
        eFood
        foodGen

    let eTurn =
          push
            ( \key ->
                do
                  snake <- sample $ current dSnake
                  pure
                    ( mfilter (/= opposite (direction snake)) $
                        case key of
                          V.EvKey V.KLeft _ -> Just Direction.Left
                          V.EvKey V.KRight _ -> Just Direction.Right
                          V.EvKey V.KUp _ -> Just Direction.Up
                          V.EvKey V.KDown _ -> Just Direction.Down
                          _ -> Nothing
                    )
            )
            eKeyInput

    let (eMoveSnake, eFood) =
          second (void . ffilter id) $
            splitE $
              pushAlways
                ( \newDirection -> do
                    snake <- sample $ current dSnake
                    food <- sample bFood

                    let newSnake = advanceHead $ snake {direction = newDirection}
                    let isFoodEaten = Snake.head newSnake == food

                    pure (applyWhen (not isFoodEaten) (fromJust . shrinkTail) newSnake, isFoodEaten)
                )
                (leftmost [eTurn, tag (direction <$> current dSnake) eTick])

    let eDied =
          void
            $ ffilter
              ( \snake@Snake {head = head@(y, x), spine} ->
                  Set.member head (Set.fromList $ tail $ snakeBody snake) || (y < 0 || y >= height || x < 0 || x >= width)
              )
            $ updated dSnake

    col . grout (fixed (constDyn height)) . row . grout (fixed (constDyn width)) $ do
      fillBoard
        ( \(x, y) -> do
            snake <- current dSnake
            food <- bFood
            if
              | (x, y) `elem` snakeBody snake -> pure (withForeColor defAttr yellow, 'O')
              | food == (x, y) -> pure (withForeColor defAttr brightGreen, '@')
              | otherwise -> pure (defAttr, '.')
        )

    pure eDied

height :: Int
width :: Int
(height, width) = (20, 20)

initialSnake :: Snake
initialSnake = Snake (height `div` 2, width `div` 2) Direction.Right [(Direction.Right, 2)]

type VtyExample t m =
  ( MonadFix m,
    MonadHold t m,
    Reflex t,
    HasInput t m,
    HasImageWriter t m,
    HasDisplayRegion t m,
    HasFocus t m,
    HasFocusReader t m,
    HasTheme t m,
    MonadNodeId m,
    HasLayout t m,
    MonadFix m,
    MonadHold t m,
    PerformEvent t m,
    TriggerEvent t m,
    MonadIO (Performable m),
    PostBuild t m,
    MonadIO m,
    Adjustable t m,
    NotReady t m
  )
