{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}


module Types where

import qualified Linear as L
import Linear.V2

import Numeric.Natural
import Data.Fixed ( Centi )

import qualified Graphics.Vty as V

import Control.Lens ( makeLenses, (^.), (.~), (&) )

data HP = HP Integer Integer
data XP = XP {
    _xp_XP :: Integer,
    _xp_max_XP :: Integer,
    _level_XP :: Integer
}
$(makeLenses ''XP)
newtype Money = Money { getMoney :: Centi }
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
    _money_Mob :: Money,
    _xp_Mob :: XP,
    _ascii_Mob :: Char
}
$(makeLenses ''Mob)

data ItemClass = Sword

data Item = Item {
    _pos_Item :: Z2
}
$(makeLenses ''Item)

data Wall = Wall {
    _pos_Wall :: Z2
}
$(makeLenses ''Wall)

data Room = Room {
    _size_Room :: N2,
    _entities_Room :: [AnyEntity]
}
$(makeLenses ''Room)

data Direction = N_Dir | NE_Dir | E_Dir | SE_Dir | S_Dir | SW_Dir | W_Dir | NW_Dir 


data Game = Game {
    _player :: Mob,
    _room :: Room
}
$(makeLenses ''Game)
