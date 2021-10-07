{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Pong where

import Control.Lens

type Game = StateT Pong Curses ()

data Point = Point { _x :: Int
                   , _y :: Int
                   } deriving (Eq, Show)

type Position = Point
 
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
                 , _playFieldSpecs :: PlayFieldSpecs
                 , _score :: (Int, Int)
                 , _speed :: Int
                 } deriving (Eq, Show)

data ColorRGB = RGB { _red :: Int
                    , _blue :: Int
                    , _green :: Int } deriving (Eq, Show)

data ColorHSV = HSV { _hue :: Int
                    , _saturation :: Int
                    , _value :: Int } deriving (Eq, Show)

data ColorHex = Int 

data Color = ColorRGB Int Int Int | ColorHSV Int Int Int | ColorHex Int deriving (Eq, Show)

data Colors = Colors { _background :: Color
                     , _foreground :: Color } deriving (Eq, Show)

data PlayFieldSize = PlayFieldSize { _width :: Integer
                                   , _height :: Integer
                                   , _resolution :: Int } deriving (Eq, Show)

data PlayFieldSpecs = PlayFieldSpecs { _size :: PlayFieldSize
                                     , _color :: Colors } deriving (Eq, Show)

data PlayFieldObjects = PlayfieldObjects { _object :: PlayFieldObject
                                         , _position :: Position
                                         , _velocity :: Velocity
                                         , _visible :: Bool
                                         , _spin :: Int } deriving (Eq, Show)

data PlayFieldObject = PlayFieldObject { _name :: String
                                       , _width :: Int
                                       , _height :: Int
                                       , _color :: Color
                                       , _bumpcolor :: Color } deriving (Eq, Show)

makeFieldsNoPrefix ''Point
makeFieldsNoPrefix ''Velocity
makeFieldsNoPrefix ''Ball
makeFieldsNoPrefix ''Paddle
makeFieldsNoPrefix ''Pong
makeFieldsNoPrefix ''ColorRGB
makeFieldsNoPrefix ''ColorHSV
makeFieldsNoPrefix ''PlayFieldSize
makeFieldsNoPrefix ''PlayFieldSpecs
makeFieldsNoPrefix ''PlayFieldObjects
makeFieldsNoPrefix ''PlayFieldObject

ballX :: Lens' Pong Int
ballX = ball . position . x

ballY :: Lens' Pong Int
ballY = ball . position . y

ballDx :: Lens' Pong Int
ballDx = ball . velocity . dx

ballDy :: Lens' Pong Int
ballDy = ball . velocity . dy

playFieldWidth :: Lens' Pong Integer
playFieldWidth = playFieldSpecs . size . width

playFieldHeight :: Lens' Pong Integer
playFieldHeight = playFieldSpecs . size . height

player1Score :: Lens' Pong Int
player1Score = score . _1

player2Score :: Lens' Pong Int
player2Score = score . _2

playFieldSpeed :: Lens' Pong Int
playFieldSpeed = speed

leftPaddle :: Lens' Pong Paddle
leftPaddle = paddles . _1

leftPaddleX :: Lens' Pong Int
leftPaddleX = leftPaddle . position . x

leftPaddleY :: Lens' Pong Int
leftPaddleY = leftPaddle . position . y

leftPaddleHeight :: Lens' Pong Int
leftPaddleHeight = leftPaddle . height

rightPaddle :: Lens' Pong Paddle
rightPaddle = paddles . _2

rightPaddleX :: Lens' Pong Int
rightPaddleX = rightPaddle . position . x

rightPaddleY :: Lens' Pong Int
rightPaddleY = rightPaddle . position . y

rightPaddleHeight :: Lens' Pong Int
rightPaddleHeight  = rightPaddle . height

paddleX :: Lens' Paddle Int
paddleX = position . x

paddleY :: Lens' Paddle Int
paddleY = position . y

data KeyAction = NoAction | Quit | LPaddleUp | LPaddleDn | RPaddleUp | RPaddleDn | Restart | Pause | Faster | Slower deriving (Eq, Show) 

