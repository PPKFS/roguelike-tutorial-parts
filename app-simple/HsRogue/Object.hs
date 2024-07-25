module HsRogue.Object where


import Rogue.Geometry.V2
import Rogue.Colour


{-
whilst the original tutorial has the "entity" as its generic game object, because we have immutability (and therefore cannot
do direct references between objects), we instead use "entity" to refer to a unique ID of an object. This means we can make links
between objects that are always up to date, such as if we want items to keep track of whatever is holding them.

this also better mimics the naming conventions used by entity component system (ECS) architectures, where entities are simply tags
associated with a loose bag of component parts.

but, mostly the distinction in here for our purposes is because haskell data is immutable.
-}

-- because we don't want to confuse normal integers with our object IDs we use a newtype wrapper here.
-- whilst it would be nicer to then use newtype wrapped entities for indexing into our maps of objects,
-- it's a little bit fiddly.
newtype Entity = Entity { unEntity :: Int }



data Object = Object
  { objectId :: Entity
  -- we also jump ahead a little, and gather together the x and y coordinates into one type and the character + colour into one type
  -- just to make it a little cleaner!
  , position :: V2
  , renderable :: Renderable

  }

-- a renderable is shared between objects (monsters, the player, items) and tiles (walls, floors, etc) so it makes sense to extract the
-- common data to a renderable type.
-- we don't want to do something OOP based, such as having the renderable type "knowing" how to render itself! there's no need to force
-- all of our renderable things into one big list.
data Renderable = Renderable
  { character :: Char
  , foregroundColour :: Colour
  , backgroundColor :: Maybe Colour
  }