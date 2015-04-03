module Radio.Field where

import Haste.Graphics.Canvas
import Haste.Perch hiding (head)
import Haste.HPlay.View hiding (head)
import Prelude hiding(id)
import Control.Monad.IO.Class

import Radio.Grid
import Radio.Tower 

fieldConfig :: (Int, Int) -> Double -> [Tower] -> Widget ()
fieldConfig (xsize, ysize) cellSize towers = do
  let g = grid xsize ysize cellSize -- get render 
      gridOffset = (cellSize, cellSize)
      scaledText s pos t = translate pos $ scale (s, s) $ text (0, 0) t
      tw t = cellSize * (fromIntegral $ length (show t) - 1)
      xlabels = mapM_ (\x -> scaledText 4 (fromIntegral x * cellSize + 0.25*cellSize - 0.3*(tw x), 0.8*cellSize) $ show x) [1 .. xsize]
      ylabels = mapM_ (\y -> scaledText 4 (0.3*cellSize-0.3*(tw y), fromIntegral y * cellSize + 0.8*cellSize) $ show y) [1 .. ysize]
      viewWidth = fst gridOffset + cellSize * fromIntegral xsize + 5
      viewHeight = snd gridOffset + cellSize * fromIntegral ysize + 5
      margin = 0.1
      scaleToCell = scale (1, 1 - 2*margin)
      placeToCell = translate (0.35*cellSize/2, margin*cellSize)
      drawTower t = do
        placeToCell $ scaleToCell $ tower (0.65*cellSize, cellSize) (RGB 0 0 0)
        translate (cellSize/2, cellSize/2) $ stroke $ circle (0, 0) (cellSize * fromIntegral (towerRadius t))
      placeTower t = translate (fst gridOffset + fromIntegral (towerX t) * cellSize
                              , snd gridOffset + fromIntegral (towerY t) * cellSize)
      drawTowers = mapM_ (\t -> placeTower t $ drawTower t) towers
  wraw (canvas ! id "fieldCanvas" 
                -- ! style "border: 1px solid black;" 
                ! atr "width" (show viewWidth)
                ! height (show viewHeight)
                $ noHtml)
    `fire` OnClick
  wraw $ liftIO $ do
      Just can <- getCanvasById "fieldCanvas"
      render can $ do
          translate gridOffset $ drawGrid g
          xlabels
          ylabels
          drawTowers
  evdata <- getEventData
  wraw $ p << ( (evName evdata) ++" fgdg"++ show (evData evdata))
