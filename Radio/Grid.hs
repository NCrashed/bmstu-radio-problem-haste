module Radio.Grid where

import Haste.Graphics.Canvas
import Prelude hiding(id)

data Grid = Grid {
  drawGrid :: Picture (),
  pixelToCell :: (Int, Int) -> (Int, Int),
  cellToPixel :: (Int, Int) -> Point
}

grid :: Int -> Int -> Double -> Grid
grid xsize ysize cellSize = Grid {
    drawGrid = makeColumns
  , pixelToCell = \(x, y) -> (
      floor (fromIntegral x / cellSize), 
      floor (fromIntegral y / cellSize))
  , cellToPixel = \(cx, cy) -> (
      fromIntegral cx * cellSize, 
      fromIntegral cy * cellSize)
  }
  where
    width = fromIntegral xsize * cellSize  
    height = fromIntegral ysize * cellSize
    
    makeColumns = mapM_ (\row -> makeLine row) [0 .. ysize-1]
    makeLine row = mapM_ (\col -> cell row col) [0 .. xsize-1]
      where 
        cell x y = 
          let dx = fromIntegral x * cellSize
              dy = fromIntegral y * cellSize
          in translate (dx, dy) $  stroke $ rect (0, 0) (cellSize, cellSize)