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

-- define our screen size in tiles.
screenSize :: V2
screenSize = V2 100 50

-- define the starting location for the '@' we draw
initialPlayerPosition :: V2
initialPlayerPosition = V2 20 20

-- our Game monad will be a state monad with the current state of the world as we play
-- on top of the IO monad to handle reading the player's inputs and drawing things to the screen
type Game a = StateT WorldState IO a

-- later this will include things like all the enemies and items in the world and the map
-- but for now we only need to store the player's position and whether we should quit the game
-- on the next loop
data WorldState = WorldState
  { playerPosition :: V2
  , pendingQuit :: Bool
  }

main :: IO ()
-- evalState is the "run this computation and return the *value* it computes" state evaluator.
main = flip evalStateT (WorldState initialPlayerPosition False) $ do
  -- withWindow is a wrapper around bracket that handles opening and closing a window
  -- but also makes sure that if we hit an exception (e.g. a SIGKILL or crash) then it
  -- will still clean up used memory and close the window gracefully.
  withWindow
    defaultWindowOptions { size = Just screenSize } -- we use the default window options, except we specify the screen size
    (return ()) -- initialisation logic: we don't have any, so do nothing extra (withWindow opens the window for us)
    (\_init -> runLoop) -- our game loop here. note that it only runs this once, so we have to do the looping ourselves
    (return ()) -- cleanup logic: we also have no extra cleanup logic so do nothing extra (withWindow closes the window for us)

-- we want to separate inputs (WASD or arrows) from the actual logic of compass directions
data Direction = LeftDir | RightDir | UpDir | DownDir
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- and because we have separated movement inputs from movement directions, we have a map to connect the two
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

-- check if a keycode corresponds to moving in a given direction. Just direction means that the button pressed
-- maps to a given direction, and Nothing means that it's not a movement key (for example, pressing "i" should do
-- nothing right now).
asMovement :: Keycode -> Maybe Direction
asMovement k = k `M.lookup` movementKeys

-- given a direction and a point, calculate the new point that is 1 tile in that direction.
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

-- as this is the simple version, I don't want to bring in optics so we'll have some helper
-- functions for modifying components of a V2

modifyX :: (Int -> Int) -> V2 -> V2
modifyX f (V2 x y) = V2 (f x) y
modifyY :: (Int -> Int) -> V2 -> V2
modifyY f (V2 x y) = V2 x (f y)

-- this is our game loop. we want to draw the screen, then handle any events (keyboard/mouse input and window
-- resizing or closing), and then finally do any game logic updates.
runLoop :: Game ()
runLoop = do
  -- rendering --
  -- first, we want to clear the screen. if we skip this, it will just re-draw each frame on top of the existing
  -- ones..which means we'll have a big snake of @s across the screen.
  terminalClear
  -- we query the game state for the current position of the player
  playerPos <- gets playerPosition
  -- we then draw an @ to the screen with terminalPrintText. this takes the position as two ints;
  -- terminalPrintText :: Int -> Int -> ...
  -- but we have a V2, so we can use withV2 to curry it for us.
  -- we could also have done
  -- (V2 playerX playerY) <- gets playerPosition
  -- _ <- terminalPrintText playerX playerY "@"
  -- printText returns the dimensions of the rectangle of the text we just drew.
  -- whilst we could just ignore this and do `withV2 ...`, it does throw a warning that we are discarding
  -- a result of type Dimensions. Using `void` is another option!

  -- NOTE: this works with Text rather than String, which is why we have OverloadedStrings enabled.
  _ <- withV2 playerPos terminalPrintText "@"
  -- now we've drawn everything, we call refresh to actually push our changes to be drawn.
  terminalRefresh

  -- event handling
  -- bearlibterminal uses a queue of events and we want to handle every event that came in since the last update
  -- handleEvents takes a blocking mode (explained shortly) and an event handling function. then for every event
  -- in the queue, it calls the handling function. there could be multiple of these events (it's very unlikely there
  -- would be multiple keypress events, but for instance there could be multiple window resizing events).
  -- the blocking mode can either be Blocking, which requires there to be at least one event processed,
  -- or NonBlocking, which will simply do nothing if there are no events. If there are no events and we are in
  -- Blocking mode, then this will wait until an event comes in. This gives us a turn based system for free! (which
  -- will be handy when we add things like monster AI because otherwise the monsters will get a turn every frame)
  _ <- handleEvents Blocking $ \case
    -- if we get a WindowClose event, then we want to stop running our game loop.
    -- but rather than doing a break or jump, it's nicer to just record that we are expecting to quit at the end of
    -- this loop.
    WindowEvent WindowClose -> modify (\worldState -> worldState { pendingQuit = True})
    -- we'll also listen for the Esc key as another way to quit
    Keypress TkEsc -> modify (\worldState -> worldState { pendingQuit = True})
    -- if we get another keypress that isn't Esc, we want to check if it's bound to moving in a direction
    Keypress other -> case asMovement other of
      -- success, the key pressed was indeed a movement direction. we update the player's position
      Just dir -> modify (\worldState ->
        worldState
          { playerPosition = calculateNewLocation dir (playerPosition worldState)
          })
      -- it wasn't an Esc, or a movement direction pressed, so we don't do anything.
      Nothing -> return ()
    -- if we get another kind of event (either a window resizing or a mouse input) we just want to ignore it for now
    _ -> return ()

  -- game logic
  -- we have none!

  -- completing the loop
  -- we check if we are expecting to quit, and if we are NOT expecting to quit then we call runLoop again
  -- and thanks to tail-call optimisation, this is nice and fast.
  -- if shouldContinue is false, then the function completes and it doesn't recurse.
  shouldContinue <- not <$> gets pendingQuit
  when shouldContinue runLoop