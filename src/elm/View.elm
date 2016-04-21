module View where

import Color            exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List             exposing (..)
import Root             exposing (..)
import Types            exposing (Ship)
import DrawLander       exposing (..)


stars : Form
stars = 
  toForm 
  <|image 500 500 
  <|root ++ "stars.png"


setUp : (Int, Int) -> Form
setUp (w,h) =
  toForm
  <|collage w h
    [ move (-250,  250) stars
    , move ( 250,  250) stars
    , move ( 250, -250) stars
    , move (-250, -250) stars
    ]


reposition : (Int, Int) -> Ship -> Form -> Form
reposition (w,h) s world = 
  toForm
  <|collage w h
    [ move (-s.x,-s.y) world ]


reorient : (Int, Int) -> Ship -> Form -> Element
reorient (w,h) s world =
  collage w h
  [ rotate (degrees -s.a) world
  , toForm 
    <|image 501 501 
    <|root ++ "scope.png"
  , blinders (w,h)
  , drawLander s
  ]


bar : Float -> Float -> (Float, Float) -> Form
bar w h cor =
  rect w h
  |>filled (rgb 14 30 95)
  |>move cor


blinders : (Int, Int) -> Form
blinders (w,h) =
  let
    vBarHeight = (toFloat (h - 501))/2 + 1
    hBarWidth  = (toFloat (w - 501))/2 + 1
    hBarHeight = (toFloat h) - (vBarHeight * 2) + 2
    w'         = toFloat w
    h'         = (toFloat h)/2
  in
    toForm
    <|collage w h
      [ bar w'        vBarHeight (0, (h' - vBarHeight/2))
      , bar w'        vBarHeight (0, (-h' + vBarHeight/2)) 
      , bar hBarWidth hBarHeight (-(w' - 500)/2 - 15, 0)
      , bar hBarWidth hBarHeight ((w'- 500)/2 + 15, 0)
      ]


view : (Int, Int) -> Ship -> Element
view d s =
  setUp (1000,1000)
  |>reposition (1000,1000) s
  |>reorient d s





