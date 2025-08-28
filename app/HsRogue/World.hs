module HsRogue.World
  ( addActor
  , WorldState(..)
  , getPlayer
  , updateActor
  ) where

import HsRogue.Prelude

import Data.Coerce (coerce)
import HsRogue.Map hiding (renderable)
import HsRogue.Actor

import HsRogue.Renderable
import Rogue.Monad ( MonadRogue, makeObject )
import Rogue.Objects.Entity ( Entity(..), HasID(..) )
import Rogue.Objects.Object ( Object(..), ObjectKind(..) )
import Rogue.Objects.Store ( Store, unsafeLookup, update, insert )

data WorldState = WorldState
  { player :: ActorEntity
  , tileMap :: Tiles
  , actors :: Store Actor
  , pendingQuit :: Bool
  } deriving (Generic)

addActor :: (MonadState WorldState m, MonadRogue m) => Text -> Renderable -> V2 -> m ActorEntity
addActor name r pos = do
  let objectData = ActorData
        { position = pos
        , renderable = r
        }
  o <- makeObject (ObjectKind "actor") name objectData ()
  acStore <- gets actors
  let newStore = insert (objectId o) o acStore
  modify (\w -> w { actors = newStore })
  return (ActorEntity (objectId o))

getPlayer :: MonadState WorldState m => m Actor
getPlayer = do
  w <- get
  return $ unsafeLookup (coerce $ player w) (actors w)

updateActor :: (MonadState WorldState m, HasID a) => a -> (Actor -> Actor) -> m ()
updateActor a f = modify (\w -> w { actors = update (coerce $ getID a) f (actors w) })
