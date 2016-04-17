import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Set
import Debug
import List exposing (..)
import Random
import Thrusters exposing (deltaY, deltaX, deltaAngular)
import View exposing (view)
import Types exposing (..)
import KeyCodes
import Task
import Effects exposing (..)
import Set exposing (Set)

-- The landers name is Reasey
reasey : Ship
reasey = 
  { x            = 0
  , y            = 0
  , a            = 0
  , vx           = 0
  , vy           = 0
  , va           = 0
  --, vx           = -0.4
  --, vy           = 3
  --, va           = 0.5
  , thrusters    =
    { leftFront  = 0
    , leftSide   = 0
    , leftBack   = 0
    , main       = 0
    , rightFront = 0
    , rightSide  = 0
    , rightBack  = 0
    }
  }


keyPressed : Int -> Set Int -> Int
keyPressed key keys =
  if Set.member key keys then
    1
  else
    0


setThrusters : Set Int -> Ship -> Ship
setThrusters keys s =
  -- Buttons correspond to direction of thrust 
  -- not thruster position on craft
  { s |
    thrusters = 
      { leftFront  = keyPressed KeyCodes.c     keys
      , leftSide   = keyPressed KeyCodes.s     keys
      , leftBack   = keyPressed KeyCodes.e     keys
      , main       = keyPressed KeyCodes.space keys
      , rightFront = keyPressed KeyCodes.n     keys
      , rightSide  = keyPressed KeyCodes.k     keys
      , rightBack  = keyPressed KeyCodes.u     keys
      }
  }


thrust : Ship -> Ship
thrust reasey =
  { reasey |
    vy = reasey.vy + 0.5 * (deltaY       reasey)
  , vx = reasey.vx + 0.5 * (deltaX       reasey)
  , va = reasey.va + 0.5 * (deltaAngular reasey)
  }

gravity : Float -> Ship -> Ship
gravity dt reasey =
  { reasey |
    vy = reasey.vy - dt/50
  , vx = reasey.vx - dt/94
  }

physics : Float -> Ship -> Ship
physics dt reasey =
  { reasey |
    y = reasey.y + dt * reasey.vy
  , x = reasey.x + dt * reasey.vx
  , a = reasey.a + dt * reasey.va
  }


update : (Float, Set Int) -> Ship -> Ship
update (dt, keys) reasey =
  reasey
    |>setThrusters keys
    --|> gravity dt
    |>thrust
    |>physics dt


main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update reasey input)


input : Signal (Float, Set Int)
input =
  let 
    delta = Signal.map (\t -> t/40) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.keysDown)


