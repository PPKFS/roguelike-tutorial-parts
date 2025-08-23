{-# LANGUAGE RecordWildCards #-}
module Main where

import HsRogue.Prelude

import Data.List.NonEmpty ( NonEmpty(..) )

import BearLibTerminal ( terminalClear, terminalRefresh, terminalSet_ )
import BearLibTerminal.Keycodes

import Rogue.Array2D.Boxed ( (!?@), traverseArrayWithCoord_ )
import Rogue.Colour ( terminalBkColour, terminalColour, black )
import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Events ( BlockingMode(..), handleEvents_ )
import Rogue.Geometry.Rectangle ( centre )
import Rogue.Monad ( MonadRogue )
import Rogue.Objects.Entity ( Entity(..) )
import Rogue.Objects.Store ( emptyStore )
import Rogue.Rendering.Print ( printChar )
import Rogue.Window ( withWindow )

import HsRogue.Map
import HsRogue.MapGen
import HsRogue.Actor
import HsRogue.Renderable
import HsRogue.World
import qualified Data.Map as M

screenSize :: V2
screenSize = V2 100 50

initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

type GameMonad m = (MonadRogue m, MonadIO m, MonadState WorldState m)

main :: IO ()
main =
  withWindow
    defaultWindowOptions { size = Just screenSize, title = Just "HsRogue - Part 3a"  }
    initGame
    (evalStateT runLoop)
    (return ())

initGame :: (MonadIO m, MonadRogue m) => m WorldState
initGame = do
  terminalSet_ "font: KreativeSquare.ttf, size=16x16"
  (madeMap, firstRoom:|_) <- roomsAndCorridorsMap 30 4 12 screenSize
  let addObjectsToWorld = do
        p <- addActor "player" playerRenderable (centre firstRoom)
        modify (\w -> w { player = p })
      initialWorld = (WorldState
        { tileMap = Tiles madeMap black
        , pendingQuit = False
        , actors = emptyStore
        , player = ActorEntity (Entity (-1))
        })
  execStateT addObjectsToWorld initialWorld

data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

movementKeys :: M.Map Keycode Direction
movementKeys = M.fromList
  [ (TkA, LeftDir)
  , (TkS, DownDir)
  , (TkW, UpDir)
  , (TkD, RightDir)
  , (TkUp, UpDir)
  , (TkDown, DownDir)
  , (TkLeft, LeftDir)
  , (TkRight, RightDir)
  ]

asMovement :: Keycode -> Maybe Direction
asMovement k = k `M.lookup` movementKeys

calculateNewLocation :: Direction -> V2 -> V2
calculateNewLocation dir (V2 x y) = case dir of
  LeftDir -> V2 (x-1) y
  RightDir -> V2 (x+1) y
  UpDir -> V2 x (y-1)
  DownDir -> V2 x (y+1)

pendQuit :: MonadState WorldState m => m ()
pendQuit = modify (\worldState -> worldState { pendingQuit = True })

runLoop :: GameMonad m => m ()
runLoop = do
  terminalClear
  renderMap
  renderActors

  terminalRefresh
  handleEvents_ Blocking $ \case
    TkClose -> pendQuit
    TkEscape -> pendQuit
    other -> case asMovement other of
      Just dir -> do
        w <- get
        playerObject <- getPlayer
        let potentialNewLocation = calculateNewLocation dir (actorPosition playerObject)
            tileAtLocation = tiles (tileMap w) !?@ potentialNewLocation
        case tileAtLocation of
          Just t
            | walkable t -> updateActor playerObject (moveActor potentialNewLocation)
          _ -> return ()
      Nothing -> return ()
  shouldQuit <- gets pendingQuit
  unless shouldQuit runLoop

renderMap :: GameMonad m => m ()
renderMap = do
  w <- get
  terminalBkColour (defaultBackgroundColour (tileMap w))
  traverseArrayWithCoord_ (tiles (tileMap w)) $ \p Tile{..} -> do
    terminalColour (foreground renderable)
    printChar p (glyph renderable)

renderActors :: GameMonad m => m ()
renderActors = do
  w <- get
  forM_ (actors w) $ \actor -> do
    let r = actorRenderable actor
    terminalColour (foreground r)
    printChar (actorPosition actor) (glyph r)
