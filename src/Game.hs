{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}


module Game where

import Types

import qualified Linear as L
import Linear.V2

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map 
import Data.List

import qualified Control.Lens as Lens
import Control.Lens ( makeLenses, (^.), (.~), (&) )
import qualified Brick as B
import qualified Brick.Widgets.Border.Style as BWBS
import qualified Brick.Widgets.Border as BWB
import qualified Graphics.Vty as V
import Brick.Widgets.Core ( (<=>), (<+>) )

instance ToVTY AnyEntity where
    toVTY (AnyEntity a) = toVTY a
instance Entity AnyEntity where
    pos (AnyEntity a) = pos a
    hp (AnyEntity a) = hp a

instance ToVTY HP where
    toVTY (HP cur max) = V.string V.defAttr $ "HP" ++ show cur ++ "/" ++ show max

instance Entity Mob where
    pos = _pos_Mob
    hp = _hp_Mob
instance ToVTY Mob where
    toVTY mob = V.char V.defAttr $ mob^.ascii_Mob

instance ToVTY Room where
    toVTY room = V.vertCat [V.horizCat [imAt (V2 x y) coordMap | x <- [0,1..fromIntegral(room^.size_Room._x)]] | y <- [0,1..fromIntegral(room^.size_Room._y)]]
        where
            coordMap :: Map Z2 V.Image 
            coordMap = foldl 
                (\acc ent -> Map.insert (pos ent) (toVTY ent) acc) 
                (Map.empty) 
                (room^.entities_Room)
            imAt :: Z2 -> Map Z2 V.Image -> V.Image
            imAt z@(V2 x y) = Map.findWithDefault (V.char V.defAttr ' ') z

initialGame :: Game
initialGame = Game {
    _player = Mob {
        _pos_Mob = V2 0 0,
        _hp_Mob = HP 10 10,
        _ascii_Mob = '@'
    },
    _room = Room {
        _size_Room = V2 50 50,
        _entities_Room = []
    }
}

renderMapPanel :: Game -- the game to render
               -> Z2 -- offset
               -> B.Widget String
renderMapPanel g offset = B.Widget B.Greedy B.Greedy $ do
    c <- B.getContext 
    let roomWithPlayer = (g^.room) { _entities_Room = (AnyEntity $ g^.player):(g^.room.entities_Room) }
        im = toVTY roomWithPlayer
        imOffset = V.translate (fromInteger $ offset^._x) (fromInteger $ offset^._y) im
        imFinal = V.resize (c^.B.availWidthL) (c^.B.availHeightL) imOffset
    return $ B.emptyResult & B.imageL .~ imFinal

renderStatsPanel :: Game
                 -> B.Widget String
renderStatsPanel g = B.padLeftRight 2 . B.padBottom B.Max . B.raw . toVTY $ g^.player.hp_Mob

renderLogPanel :: Game -> B.Widget String
renderLogPanel g = B.padRight B.Max . B.padAll 1 $ B.str "WriterT [V.Image] (B.EventM String (B.Next G.Game)) ()"

renderGame :: Game -> B.Widget String
renderGame g = B.joinBorders $ (worldmap <=> log) <+> stats
    where
        frameWithTitle title = B.withBorderStyle BWBS.unicodeRounded . BWB.borderWithLabel (B.str title)
        worldmap = frameWithTitle "W O R L D M A P" $ renderMapPanel g (pure 0)
        stats = frameWithTitle "S T A T S" $ renderStatsPanel g
        log = frameWithTitle "L O G" $ renderLogPanel g
