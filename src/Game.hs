{-# LANGUAGE TemplateHaskell #-}

module Game
    ( Game
    ) where

data Game = Game {
    _player :: Player,
    _room :: [[Entity]]
} deriving (Show)

class (Show a) => MkEntity a where
    pos :: a -> (Integer, Integer)
    hp :: a -> (Integer, Integer)

data Entity = forall a. Show a => MkEntity a