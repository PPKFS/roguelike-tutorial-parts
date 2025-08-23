---
author: ["Avery"]
title: "Haskell Roguelike Tutorial Part 3 - Objects, Actors, and Lenses"
date: "2025-07-23"
modified: "2025-07-23"
description: "In this part, we design a proper architecture for our project: we turn the player into an object (an actor, specifically) and then briefly introduce optics as a way to avoid the hell of nested record updates."
summary: "Adding a proper object and actor model and introducing the optics library."
tags: ["roguelike", "tutorial", "projects", "haskell"]
categories: ["haskell"]
series: ["roguelike-tutorial"]
ShowToc: true
TocOpen: true
draft: false
weight: 1
social:
  bluesky: "ppkfs@bsky.social"
---

**WORKING DRAFT**

Welcome back to part 3 of the Haskell roguelike tutorial! In the [previous part](https://ppkfs.github.io/posts/roguelike-tutorial/part2/) we added some modules to make tilemaps and finished up with a simple dungeon (and collision!). In this part, we're going to make a proper model of objects (such as items, or doors, or traps) and actors (like the player and enemies). In the second half, we're going to introduce *optics* with the `optics` library, which you may have heard talked about as "lenses". They are a construct that makes dealing with nested record getting and setting a huge amount easier - such as being able to apply a function to an element of a `Map` that's nested 4 types deep if the element exists!

This part will be lacking in new features, so I hope to make it enlightening in how I approach the architecture.

Again, if you missed it in the last part: this part going forward will be somewhat more brief. I want to actually finish this damn thing, and that means probably not writing 3000 words per part. I hope it's still readable and followable!

TEMPORARY NOTE: yeah in the interests of getting this at least up in skeleton form, it'll get very very patchy from here on until I delete this part of the post.

# Object Architecture Overview

Let's start with a brief overview of what we are building here. `Roguefunctor` has a barebones object model based on something I settled on when writing [`yaifl`](https://github.com/ppkfs/yaifl), my interactive fiction library. The key was that I wanted something where the object type was *extensible* from outside the library, but *also* keeping a strong sense of type safety. This is my problem with entity-component-system architectures - which are great and lots of people love them for roguelikes! I won't go into them in much depth here, because it'll just be a rant, but if you do want to try them there's at least two solid ECS libraries for Haskell: [`apecs`](https://github.com/jonascarpay/apecs) and [`aztecs`](https://github.com/aztecs-hs/aztecs).

The object model has 3 parts:
  - an `Entity`, which is just a numeric ID used to by objects to reference other objects (e.g. the current holder of an item). This is necessary to get around immutability. These can be wrapped with `newtype`s to produce references that the object it references satisfies some property (such as a `ThingEntity` meaning "this reference ID points to an item").
  - an `Object` type, representing some game object. This type is parameterised over two parameters: `specifics` and `data`. `data` represents the common record fields for a wide class of things (such as items, or living beings). `specifics` is for the individual, precise record fields (a door may have specific data like `openState` or `locked`). We'll give a small example of this below.
  - a `Store` type that is a map of `Entity`s to `Object`s.

The `Object` type is the most important part and may need a bit more explaining. The following `Object` type, from `Rogue.Object`:

```haskell
data Object objSpecifics objData = Object
  { name :: Text
  , objectId :: Entity
  , objectKind :: ObjectKind
  , creationTime :: Timestamp
  , modifiedTime :: Timestamp
  , objectData :: objData
  , specifics :: objSpecifics
  } deriving stock (Generic)
```
In this tutorial, we're going to have 3 broad classes of objects:

- *Actors* - living beings that move around or fight or chat. The player is an actor, enemies like goblins are actors, NPCs are actors.
- *Items* - objects on the ground, equipped by some actor, in someone's inventory, etc.
- *TileEntities* - objects that do stuff, but aren't items to be picked up. They can be interacted with - such as a door, or a trap, or a torch fixed to the wall.

We'll also have various types that represent individual versions of each of these - such as `data DoorSpecifics = DoorSpecifics OpenOrClosed LockingInfo` or `data SwordSpecifics = SwordSpecifics Sharpness Damage`. We can make these into a big ol' sum type that defines our game logic in one place: `data ObjectSpecifics = DoorS DoorSpecifics | SwordS SwordSpecifics`. (Yes, this is a little bit clunky...we could of course make these into a hierarchy so we have a slightly less huge sum type, but you're going to have something similar no matter if you use ECS or not. But I digress)

Our game objects for the specific game we're making will now look like `type GameObject objData = Object ObjectSpecifics objData` - one parameter down. The final part is that for each broad class above, each of these will correspond to a *different instantiation of `objData`*; that is, if we have some types that contain the common properties for all "actors" called `ActorSpecifics` (perhaps a position, some combat stats, an inventory) then we can define `type Actor = GameObject ActorSpecifics`! We can make a `Store Actor` to hold all the actors in our game. We can define functions that work on `Actor` types with all the nice type safety you expect from Haskell.

We can go one step further (safer): we define a newtype wrapper around `Entity` such as `newtype ActorEntity = ActorEntity Entity` and not export the constructor. Then, we define the only way to make an `ActorEntity` to go via `tagActor :: Actor -> ActorEntity`. We can now store object references that are guaranteed to have the correct type when resolved!

# Object Module

We will start with a simpler set of object types: we don't have items or tile entities yet, only actors (and only one actor - the player - until part 5). Every actor will have a position and a renderable.


All of World

```haskell
module HsRogue.World
  ( addActor
  , WorldState(..)
  , getPlayer
  , updateActor
  ) where

import HsRogue.Prelude

import Data.Coerce (coerce)
import HsRogue.Map hiding (renderable)
import HsRogue.Object

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
  let objectData = ObjectData
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
```

```haskell
import Rogue.Objects.Entity ( Entity(..) )
import Rogue.Objects.Store ( emptyStore )
import Rogue.Rendering.Print ( printChar )

import HsRogue.Object

import HsRogue.World

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

runLoop :: GameMonad m => m ()
runLoop = do
  terminalSet_ "font: KreativeSquare.ttf, size=16x16"
  terminalClear
  renderMap
  renderActors

Just dir -> do
        w <- get
        playerObject <- getPlayer
        let potentialNewLocation = calculateNewLocation dir (objectPosition playerObject)
            tileAtLocation = tiles (tileMap w) !?@ potentialNewLocation
        case tileAtLocation of
          Just t
            | walkable t -> updateActor playerObject (moveObject potentialNewLocation)

renderActors :: GameMonad m => m ()
renderActors = do
  w <- get
  forM_ (actors w) $ \actor -> do
    let r = objectRenderable actor
    terminalColour (foreground r)
    printChar (objectPosition actor) (glyph r)

```