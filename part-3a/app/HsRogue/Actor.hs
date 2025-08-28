module HsRogue.Actor
  ( actorRenderable
  , actorPosition
  , moveActor
  , Actor
  , ActorEntity(..)
  , ActorData(..)
  ) where

import HsRogue.Prelude
import HsRogue.Renderable
import Rogue.Objects.Object as RF ( Object(..) )
import Rogue.Objects.Entity ( Entity(..) )

data ActorData = ActorData
  { position :: V2
  , renderable :: Renderable
  }

type Actor = RF.Object () ActorData

newtype ActorEntity = ActorEntity { unActor :: Entity }
  deriving (Eq, Ord, Show, Enum)

actorRenderable :: RF.Object a ActorData -> Renderable
actorRenderable = renderable . objectData

actorPosition :: RF.Object a ActorData -> V2
actorPosition = position . objectData

moveActor :: V2 -> RF.Object a ActorData -> RF.Object a ActorData
moveActor pos o = o { objectData = (objectData o) {position = pos } }
