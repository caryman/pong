{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Pong where

import Control.Lens

data Point = Point { _x :: Int
                   , _y :: Int
                   } deriving (Eq, Show)

data Velocity = Velocity { _dx :: Int
                         , _dy :: Int
                         } deriving (Eq, Show)

data Ball = Ball { _position :: Point
                 , _velocity :: Velocity
                 } deriving (Eq, Show)

data Paddle = Paddle { _position :: Point
                     , _height :: Int
                     } deriving (Eq, Show)

data Pong = Pong { _ball :: Ball
                 , _paddles :: (Paddle, Paddle)
                 } deriving (Eq, Show)

makeFieldsNoPrefix ''Point
makeFieldsNoPrefix ''Velocity
makeFieldsNoPrefix ''Ball
makeFieldsNoPrefix ''Paddle
makeFieldsNoPrefix ''Pong



