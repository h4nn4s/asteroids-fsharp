#load "asteroids.fs"
open Asteroids

#r "nuget:DIKU.Canvas, 2.0"
open Canvas
open Color

let w,h = 512,512

let drawSpaceship : PrimitiveTree =
    let baseWidth, height = 20.0, 30.0
    let centerX, centerY = (float w) / 2.0, (float h) / 2.0
    let triangleVertices = 
        [ (centerX, centerY - height / 2.0); 
          (centerX - baseWidth / 2.0, centerY + height / 2.0); 
          (centerX + baseWidth / 2.0, centerY + height / 2.0) ]
    filledPolygon green triangleVertices

let drawAsteroid x y radius : PrimitiveTree =
    translate x y (filledEllipse white radius radius)

let gameBoard : Picture =
    let background = filledRectangle black (float w) (float h)
    let asteroid = drawAsteroid 40.0 100.0 20.0

    emptyTree
    |> onto background
    |> onto drawSpaceship
    |> onto asteroid
    |> make

render "Spaceship and Asteroid" w h gameBoard
