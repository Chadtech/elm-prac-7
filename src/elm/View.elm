module View where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)
import Root exposing (..)
import Types exposing (Ship)
import DrawLander exposing (..)

goodBlue : Color
goodBlue =
  rgb 14 30 95

stars : Form
stars = 
  toForm 
  <|image 500 500 
  <|root ++ "stars.png"


setUp : (Int, Int) -> Ship -> Form
setUp (w,h) s =
  let
    lander =
      rotate (degrees s.a)
      <|move (s.x, s.y) 
      <|toForm 
      <|drawLander s

    space =
      toForm
      <|collage 1000 1000
        [ move (-250,  250) stars
        , move ( 250,  250) stars
        , move ( 250, -250) stars
        , move (-250, -250) stars
        ]
  in
    toForm
    <|collage w h [ space ]


reposition : (Int, Int) -> Ship -> Form -> Form
reposition (w,h) s world = 
  toForm
  <|collage w h
    [ move (-s.x,-s.y) world ]




reorient : (Int, Int) -> Ship -> Form -> Element
reorient (w,h) s world =
  let
    lander =
      --rotate (degrees s.a)
      --<|move (s.x, s.y) 
      toForm 
      <|drawLander s
  in
    collage w h
    [ rotate (degrees -s.a) world
    , toForm <| image 501 501 (root ++ "scope.png")
    , blinders (w,h)
    , lander
    ]

blinders : (Int, Int) -> Form
blinders (w,h) =
  let
    vBarHeight = (toFloat (h - 501))/2 + 1
    hBarWidth  = (toFloat (w - 501))/2 + 1
    hBarHeight = (toFloat h) - (vBarHeight * 2) + 2
  in
    toForm
    <|collage w h
      [ move (0, ((toFloat h)/2 - vBarHeight/2))
        <|filled goodBlue 
        <| (rect (toFloat w) vBarHeight) 
      , move (0, (-(toFloat h)/2 + vBarHeight/2))
        <|filled goodBlue 
        <| (rect (toFloat w) vBarHeight) 
      , move (-((toFloat w) - 500)/2 - 15, 0)
        <|filled goodBlue 
        <| (rect hBarWidth hBarHeight) 
      , move (((toFloat w) - 500)/2 + 15, 0)
        <|filled goodBlue 
        <| (rect hBarWidth hBarHeight) 
      ]


view : (Int, Int) -> Ship -> Element
view d s =
  reorient d s
  <|reposition (1000, 1000) s
  <|setUp (1000, 1000) s





