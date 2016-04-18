import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Set
import Debug exposing (log)
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
  , a            = 15
  , vx           = 0
  , vy           = 0
  , va           = -0.1
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
  { s
  | thrusters = 
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

floatModulo: Float -> Int -> Float
floatModulo n m =
  let
    n' = 
      round n

    modulod =
      n' % m
  in
    (-) (toFloat modulod)
    <|  (toFloat n') - n


physics : Float -> Ship -> Ship
physics dt reasey =
  let 
    y' = 
      let 
        --yeeee = log "speee" reasey.vy
        y'' = 
          reasey.y + dt * reasey.vy
      in    
        --floatModulo y''  250
        if y'' > 250 then
          y'' - 500
        else
          if y'' < -250 then
            y'' + 500
          else
            y''

    x' = 
      let
        x'' = 
          reasey.x + dt * reasey.vx
      in
        if x'' > 250 then
          x'' - 500
        else
          if x'' < -250 then
            x'' + 500
          else
            x''

  in
    { reasey |
      y = y'
    , x = x'
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
    delta = Signal.map (\t -> t/120) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.keysDown)


