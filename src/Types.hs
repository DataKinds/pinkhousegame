{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}


module Types where

import qualified Linear as L
import Linear.V2

import Numeric.Natural

import qualified Graphics.Vty as V



data HP = HP Integer Integer
type Z2 = V2 Integer
type N2 = V2 Natural 

class ToVTY a where
    toVTY :: a -> V.Image

class (ToVTY a) => Entity a where
    pos :: a -> Z2
    hp  :: a -> HP
    
data AnyEntity = forall a. (Entity a, ToVTY a) => AnyEntity a 

data Mob = Mob {
    _pos_Mob :: Z2,
    _hp_Mob  :: HP,
    _ascii_Mob :: Char
}
$(makeLenses ''Mob)

data Room = Room {
    _size_Room :: N2,
    _entities_Room :: [AnyEntity]
}
$(makeLenses ''Room)

data Game = Game {
    _player :: Mob,
    _room :: Room
}
$(makeLenses ''Game)
