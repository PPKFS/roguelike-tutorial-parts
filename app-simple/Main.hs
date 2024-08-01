module Main where


import Rogue.Window ( withWindow )
import Rogue.Config ( WindowOptions(..), defaultWindowOptions )
import Rogue.Geometry.V2 ( V2(..), withV2 )
import Rogue.Events ( BlockingMode(..), handleEvents )
import qualified Data.Map as M
import BearLibTerminal
    ( Keycode(..),
      Event(..),
      WindowEvent(..),
      terminalClear,
      terminalPrintText,
      terminalRefresh )
import Control.Monad.Trans.State
import Control.Monad (when)

import HsRogue.Object
import qualified Data.IntMap as IM
import Rogue.Colour
import Data.Text (singleton)

-- this is all our existing part 1 code.

screenSize :: V2
screenSize = V2 100 50

initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

type Game a = StateT WorldState IO a

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
calculateNewLocation dir v =
  let updateIt :: V2 -> V2
      updateIt = case dir of
        LeftDir -> modifyX (subtract 1)
        RightDir -> modifyX (+1)
        UpDir -> modifyY (subtract 1)
        DownDir -> modifyY (+1)
  in
    updateIt v

modifyX :: (Int -> Int) -> V2 -> V2
modifyX f (V2 x y) = V2 (f x) y
modifyY :: (Int -> Int) -> V2 -> V2
modifyY f (V2 x y) = V2 x (f y)

{-
For part 2, the original tutorial extracts things to an "Engine" class. We don't need to do that. The Engine class in the python tutorial:
- has some game state (which we have in our State monad)
- has some methods for each part of the game loop (which we have as just regular functions)
- has some initialisation logic (which we have in our call to `withWindow`)
- an event handler (which again, is just a bunch of functions glued together. we...can just use functions)
-}
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) f = f x

data WorldState = WorldState
  { objects :: IM.IntMap Object
  , entityCounter :: Entity
  , pendingQuit :: Bool
  }

playerId :: Entity
playerId = Entity 0

main :: IO ()
main = flip evalStateT (WorldState IM.empty 0 False) $ do
  withWindow
    defaultWindowOptions { size = Just screenSize }
    (return ())
    (\_init -> do
      _ <- buildWorld
      runLoop)
    (return ())

addObject :: V2 -> Renderable -> Game ()
addObject pos renderable = do
  e <- gets entityCounter
  objs <- gets objects
  let newObj = Object e pos renderable
  modify (\w -> w { entityCounter = e + 1, objects = IM.insert (unEntity e) newObj objs })

buildWorld :: Game ()
buildWorld = do
  addObject initialPlayerPosition (Renderable '@' (Colour 0xFFFFFFFF) Nothing)
  addObject (initialPlayerPosition - V2 5 5)) (Renderable '@' (Colour 0xFF66FF00) Nothing)

getObject :: Entity -> Game Object
getObject (Entity e) = do
  mbObject <- gets (IM.lookup e . objects)
  case mbObject of
    Just o -> return o
    Nothing -> error $ "Could not find object with ID " <> show (Entity e)

modifyObject :: Entity -> (Object -> Object) -> Game ()
modifyObject (Entity e) f = do
  objs <- gets objects
  modify (\w -> w { objects = IM.update (Just . f) e objs })

setObject :: Entity -> Object -> Game ()
setObject e o = modifyObject e (const o)

render :: Game ()
render = do
  terminalClear
  objects <- gets objects
  mapM_ renderObject objects
  terminalRefresh

renderObject :: Object -> Game ()
renderObject o = do
  let r = renderable o
  terminalColor (foregroundColour r)
  whenJust (backgroundColor r) terminalBkColor
  let (V2 x y) = position o
  _ <- terminalPrintText x y (singleton (character r))
  return ()

quitNowPending :: Game ()
quitNowPending = modify (\ws -> ws { pendingQuit = True})

keybinds :: M.Map Keycode (Game ())
keybinds = M.fromList $
  [ (TkEsc, quitNowPending)
  ] <>
  map (\(key, dir) -> (key, movePlayer dir)) (M.toList movementKeys)

movePlayer :: Direction -> Game ()
movePlayer dir = do
  p <- getObject playerId
  let newPosition = calculateNewLocation dir (position p)
  modifyObject playerId (\p' -> p' { position = newPosition })

runLoop :: Game ()
runLoop = do
  render
  _ <- handleEvents Blocking $ \case
    WindowEvent WindowClose -> quitNowPending
    WindowEvent Resize -> return ()
    Keypress key -> case M.lookup key keybinds of
      Just action -> action
      Nothing -> return ()
  shouldContinue <- not <$> gets pendingQuit
  when shouldContinue runLoop