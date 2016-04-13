module Thrusters where

import Types exposing (Ship)

deltaY : Ship -> Float
deltaY s = 
  let
    t    = s.thrusters
    cos' = cos <| degrees s.a
    sin' = sin <| degrees s.a

    main = 2.5  * cos' * toFloat t.main
    lb   = 0.1  * cos' * toFloat t.leftBack
    lf   = -0.1 * cos' * toFloat t.leftFront
    rb   = 0.1  * cos' * toFloat t.rightBack
    rf   = -0.1 * cos' * toFloat t.rightFront
    ls   = -0.1 * sin' * toFloat t.leftSide
    rs   = 0.1  * sin' * toFloat t.rightSide

  in
    ls + lb + lf + main + rf + rb + rs



deltaX : Ship -> Float
deltaX s = 
  let
    t    = s.thrusters
    cos' = cos <| degrees s.a
    sin' = sin <| degrees s.a

    main = -2.5 * sin' * toFloat t.main
    lb   = -0.1 * sin' * toFloat t.leftBack
    lf   = 0.1  * sin' * toFloat t.leftFront
    rb   = -0.1 * sin' * toFloat t.rightBack
    rf   = 0.1  * sin' * toFloat t.rightFront
    ls   = -0.1 * cos' * toFloat t.leftSide
    rs   = 0.1  * cos' * toFloat t.rightSide

  in
    ls + lb + lf + main + rf + rb + rs



deltaAngular : Ship -> Float
deltaAngular s =
  let 
    t  = s.thrusters

    lb = -0.1 * toFloat t.leftBack
    lf = 0.1  * toFloat t.leftFront
    rb = 0.1  * toFloat t.rightBack
    rf = -0.1 * toFloat t.rightFront

  in
    lb + lf + rb + rf 