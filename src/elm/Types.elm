module Types where

type alias Ship =
  { x:            Float
  , y:            Float
  , a:            Float
  , vx:           Float
  , vy:           Float
  , va:           Float
  , thrusters: 
    { leftFront:  Int
    , leftSide:   Int
    , leftBack:   Int
    , main:       Int
    , rightFront: Int
    , rightSide:  Int
    , rightBack:  Int
    }
  }