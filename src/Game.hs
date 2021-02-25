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
import Control.Lens ( makeLenses, (^.), (.~), (&), (-~) )
import Control.Monad.Trans.State.Lazy

import qualified Brick as B
import qualified Brick.Widgets.Border.Style as BWBS
import qualified Brick.Widgets.Border as BWB
import Brick.Widgets.Core ( (<=>), (<+>) )
import qualified Graphics.Vty as V

    

instance ToVTY AnyEntity where
    toVTY (AnyEntity a) = toVTY a
instance ToVTY HP where
    toVTY (HP cur max) = V.string V.defAttr $ "HP" ++ show cur ++ "/" ++ show max
instance ToVTY Money where
    toVTY (Money m) = V.string V.defAttr $ "$" ++ show m
instance ToVTY XP where
    toVTY (XP cur max lvl) = V.string V.defAttr $ "XP" ++ show cur ++ "/" ++ show max ++ " LVL" ++ show lvl 
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
instance ToVTY Wall where
    toVTY wall = V.char V.defAttr '#'

instance Entity AnyEntity where
    pos (AnyEntity a) = pos a
    hp (AnyEntity a) = hp a
instance Entity Mob where
    pos = _pos_Mob
    hp = _hp_Mob
instance Entity Wall where
    pos = _pos_Wall
    hp = const (HP 10 10)


entityWalk :: Direction -> _ -> State Game Bool
entityWalk dir lensish = do
    game <- get
    put $ case dir of
        N_Dir -> game & lensish.L._y -~ 1
    return True

initialGame :: Game
initialGame = Game {
    _player = Mob {
        _pos_Mob = V2 0 0,
        _hp_Mob = HP 10 10,
        _money_Mob = Money 150,
        _xp_Mob = XP 0 1000 1,
        _ascii_Mob = '@'
    },
    _room = Room {
        _size_Room = V2 100 25,
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
renderStatsPanel g = B.padLeftRight 2 . B.padBottom B.Max . B.vBox $ [hpLine, moneyLine, xpLine]
    where
        imgToWidget :: ToVTY a => a -> B.Widget String
        imgToWidget = B.raw . toVTY
        hpLine = imgToWidget $ g^.player.hp_Mob
        moneyLine = imgToWidget $ g^.player.money_Mob
        xpLine = imgToWidget $ g^.player.xp_Mob

renderLogPanel :: Game -> B.Widget String
renderLogPanel g = B.padRight B.Max . B.padAll 1 $ B.str "WriterT [V.Image] (B.EventM String (B.Next G.Game)) ()"

renderGame :: Game -> B.Widget String
renderGame g = B.joinBorders $ (worldmap <=> log) <+> stats
    where
        frameWithTitle title = B.withBorderStyle BWBS.unicodeRounded . BWB.borderWithLabel (B.str title)
        worldmap = frameWithTitle "W O R L D M A P" $ renderMapPanel g (pure 0)
        stats = frameWithTitle "S T A T S" $ renderStatsPanel g
        log = frameWithTitle "L O G" $ renderLogPanel g
