module View where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)
import Root exposing (..)
import Types exposing (Ship)
import DrawLander exposing (..)


setUp : (Int, Int) -> Ship -> Form
setUp (w,h) s =
  let

    lander =
      rotate (degrees s.a)
      <|move (s.x, s.y)
      <|toForm 
      <|drawLander s 

    stars = 
      toForm 
      <|image 500 500 
      <|root ++ "measure.png"

  in
    toForm
    <|collage w h [ stars, lander ]

reposition : (Float, Float) -> (Int, Int) -> Form -> Element
reposition (x,y) (w,h) world = 
  collage w h
    [ move (-x,-y) world ]

view : (Int, Int) -> Ship -> Element
view (w, h) s =
  reposition (s.x,s.y) (w,h)
  <|setUp (w, h) s
    --collage w' h'
    --[ move (-cx, -cy)
    --  <|rotate (degrees -s.a)
    --  <|toForm main
    --]

    --move (-s.x, -s.y)
    --<| rotate (degrees -s.a)
    --<| toForm main




