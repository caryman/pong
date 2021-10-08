{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Pong where

import Control.Lens

--type Game = StateT Pong Curses ()

data Ball = Ball { _position :: (Integer, Integer)
                 , _velocity :: (Integer, Integer)
                 } deriving (Eq, Show)

data Paddle = Paddle { _position :: (Integer, Integer)
                     , _height :: Integer
                     } deriving (Eq, Show)

data Pong = Pong { _ball :: Ball
                 , _paddles :: (Paddle, Paddle)
                 , _playFieldSpecs :: PlayFieldSpecs
                 , _score :: (Integer, Integer)
                 , _speed :: Int
                 } deriving (Eq, Show)

data ColorRGB = RGB { _red :: Integer
                    , _blue :: Integer
                    , _green :: Integer } deriving (Eq, Show)

data ColorHSV = HSV { _hue :: Integer
                    , _saturation :: Integer
                    , _value :: Integer } deriving (Eq, Show)

data ColorHex = Integer 

data Color = ColorRGB Integer Integer Integer | ColorHSV Integer Integer Integer | ColorHex Integer deriving (Eq, Show)

data Colors = Colors { _background :: Color
                     , _foreground :: Color } deriving (Eq, Show)

data PlayFieldSize = PlayFieldSize { _width :: Integer
                                   , _height :: Integer
                                   , _resolution :: Integer } deriving (Eq, Show)

data PlayFieldSpecs = PlayFieldSpecs { _size :: PlayFieldSize
                                     , _color :: Colors } deriving (Eq, Show)

data PlayFieldObjects = PlayfieldObjects { _object :: PlayFieldObject
                                         , _position :: (Integer, Integer)
                                         , _velocity :: (Integer, Integer)
                                         , _visible :: Bool
                                         , _spin :: Integer } deriving (Eq, Show)

data PlayFieldObject = PlayFieldObject { _name :: String
                                       , _width :: Integer
                                       , _height :: Integer
                                       , _color :: Color
                                       , _bumpcolor :: Color } deriving (Eq, Show)

makeFieldsNoPrefix ''Ball
makeFieldsNoPrefix ''Paddle
makeFieldsNoPrefix ''Pong
makeFieldsNoPrefix ''ColorRGB
makeFieldsNoPrefix ''ColorHSV
makeFieldsNoPrefix ''PlayFieldSize
makeFieldsNoPrefix ''PlayFieldSpecs
makeFieldsNoPrefix ''PlayFieldObjects
makeFieldsNoPrefix ''PlayFieldObject

_x :: Field1 s t a b => Lens s t a b
_x = _1

_y :: Field2 s t a b => Lens s t a b
_y = _2

leftPaddle :: Lens' Pong Paddle
leftPaddle = paddles . _1

rightPaddle :: Lens' Pong Paddle
rightPaddle = paddles . _2

data KeyAction = NoAction | Quit | LPaddleUp | LPaddleDn | RPaddleUp | RPaddleDn | Restart | Pause | Faster | Slower deriving (Eq, Show) 

