module Thrusters where

import Types exposing (Ship)

weakPower : Float
weakPower = 0.2

mainPower : Float
mainPower = weakPower * 15


deltaY : Ship -> Float
deltaY s = 
  let
    t    = s.thrusters
    cos' = cos <| degrees s.a
    sin' = sin <| degrees s.a

    main = mainPower  * cos' * toFloat t.main
    lb   = weakPower  * cos' * toFloat t.leftBack
    lf   = -weakPower * cos' * toFloat t.leftFront
    rb   = weakPower  * cos' * toFloat t.rightBack
    rf   = -weakPower * cos' * toFloat t.rightFront
    ls   = -weakPower * sin' * toFloat t.leftSide
    rs   = weakPower  * sin' * toFloat t.rightSide

  in
    ls + lb + lf + main + rf + rb + rs



deltaX : Ship -> Float
deltaX s = 
  let
    t    = s.thrusters
    cos' = cos <| degrees s.a
    sin' = sin <| degrees s.a

    main = -mainPower * sin' * toFloat t.main
    lb   = -weakPower * sin' * toFloat t.leftBack
    lf   = weakPower  * sin' * toFloat t.leftFront
    rb   = -weakPower * sin' * toFloat t.rightBack
    rf   = weakPower  * sin' * toFloat t.rightFront
    ls   = -weakPower * cos' * toFloat t.leftSide
    rs   = weakPower  * cos' * toFloat t.rightSide

  in
    ls + lb + lf + main + rf + rb + rs



deltaAngular : Ship -> Float
deltaAngular s =
  let 
    t  = s.thrusters

    lb = -weakPower * 0.8 * toFloat t.leftBack
    lf = weakPower  * 0.8 * toFloat t.leftFront
    rb = weakPower  * 0.8 * toFloat t.rightBack
    rf = -weakPower * 0.8 * toFloat t.rightFront

  in
    lb + lf + rb + rf 