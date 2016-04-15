module View where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)
import Root exposing (..)
import Types exposing (Ship)
import DrawLander exposing (..)


view : (Int, Int) -> Ship -> Element
view (w', h') s =
  let 

    (w, h)   = (toFloat w', toFloat h')
    position = (s.x, s.y)


    landerKeys = 
      move (100 - w/2, h/2 - 100)
      <|  toForm
      <|  image 276 276 
      <|  root ++ "thruster_keys.png"


    lander =
      rotate (degrees s.a)
      <|  move position
      <|  toForm 
      <|  drawLander s


    stars = 
      toForm 
      <|  image 500 500 
      <|  root ++ "stars.png"


    -- Make a column of the stars tile,
    -- where each tile is positioned right
    tile   : Float -> List Form
    tile t =
      let 
        pos   = ((500 * t) - (w - 500)/2 , h - 500)
        tile' = move pos stars
      in 
        List.map (\u -> (move (0, u * -495) tile'))
        <|  List.map (\n -> toFloat n) 
        <|  [ 0 .. (h' // 500) + 1 ]

    -- Make several columns and rows
    -- of the star tile, positioned
    -- according to their column
    -- and row index
    tiles = 
      List.foldr append []
      <| List.map tile 
      <| List.map (\n -> toFloat n) 
      <| [ 0 .. ((w' // 500) + 1) ]
    
  in

    collage w' h' 
    <|  append tiles 
    [ lander
    , landerKeys
    ]