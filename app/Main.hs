module Main where

import qualified Linear as L
import Linear.V2

import Types
import qualified Game as G
import qualified Brick as B
import qualified Graphics.Vty as V

import Control.Lens ( makeLenses, (^.), (.~), (&), (+~), (-~) )


type AppState = G.Game

keyHandler :: Char -> AppState -> AppState
keyHandler c g = case c of 
    'h' -> g & G.player.G.pos_Mob.L._x -~ 1
    'j' -> g & G.player.G.pos_Mob.L._y -~ 1
    'k' -> g & G.player.G.pos_Mob.L._y +~ 1
    'l' -> g & G.player.G.pos_Mob.L._x +~ 1

appHandleEvent :: AppState -> B.BrickEvent String () -> B.EventM String (B.Next AppState)
appHandleEvent g (B.VtyEvent (V.EvKey V.KEsc [])) = B.halt g
appHandleEvent g (B.VtyEvent (V.EvKey (V.KChar c) [])) = B.continue $ keyHandler c g
appHandleEvent g _ = B.continue g



app :: B.App AppState () String
app = B.App 
    { B.appDraw = return . G.renderGame
    , B.appChooseCursor = const . const Nothing
    , B.appHandleEvent = appHandleEvent
    , B.appStartEvent = return
    , B.appAttrMap = const (B.attrMap V.defAttr [])
    }

main :: IO ()
main = do
    let initialState = G.initialGame
    B.defaultMain app initialState >> mempty
