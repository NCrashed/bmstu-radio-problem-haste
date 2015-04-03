module Main where

import Haste hiding (sytle)
import Haste.Graphics.Canvas
import Haste.Perch
import Prelude hiding(id)

import Radio.Grid

main :: IO ()
main = do
  body <- getBody
  (flip build) body $ 
     center $ canvas ! id "canvas" 
                     ! style "border: 1px solid black;" 
                     ! atr "width" "320" 
                     ! height "320"
                     $ noHtml
  Just can <- getCanvasById "canvas"
  animate can 0


animate :: Canvas -> Double -> IO ()
animate can angle = do
  let testGrid = grid 5 7 50
  render can $ drawGrid testGrid