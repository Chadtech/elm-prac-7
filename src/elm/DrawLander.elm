module DrawLander where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)
import Root exposing (..)
import Types exposing (Ship)

drawLander : Ship -> Element
drawLander s =
    let
      t = s.thrusters

      lander = 
        [ toForm 
          <| image 47 48 
          <| root ++ "lander.png" 
        ]

      mainThruster = 
        if t.main == 1 then
          [ move (0, -32) 
            <| toForm 
            <| image 12 33 
            <| root ++ "blast_main.png"
          ]
        else []

      leftFront =
        if t.leftFront == 1 then
          [ move (-19, 9) 
            <| toForm  
            <| image 2 9 
            <| root ++ "blast_yaw.png"
          ]
        else []

      leftBack =
        if t.leftBack == 1 then
          [ move (-19, -9) 
            <| scale -1
            <| toForm  
            <| image 2 9 
            <| root ++ "blast_yaw_f.png"
          ]
        else []

      leftSide = 
        if t.leftSide == 1 then
          [ move (25, -1)
            <| rotate (degrees 180)
            <| toForm
            <| image 8 3 
            <| root ++ "blast_strafe.png"
          ]
        else []

      rightSide =
        if t.rightSide == 1 then
          [ move (-25, -1)
            <| rotate (degrees 0)
            <| toForm
            <| image 8 3 
            <| root ++ "blast_strafe.png"
          ]
        else []

      rightFront =
        if t.rightFront == 1 then
          [ move (19, 9) 
            <| rotate (degrees 180) 
            <| scale -1  
            <| toForm  
            <| image 2 9
            <| root ++ "blast_yaw_f.png"
          ]
        else []

      rightBack =
        if t.rightBack == 1 then
          [ move (19, -9)
            <| rotate (degrees 180)
            <| toForm
            <| image 2 9 
            <| root ++ "blast_yaw.png"
          ]
        else []

    in
      collage 138 138
      <|List.foldr append []
        [ rightSide
        , leftSide
        , rightBack
        , leftBack
        , rightFront
        , leftFront
        , mainThruster
        , lander
        ]



