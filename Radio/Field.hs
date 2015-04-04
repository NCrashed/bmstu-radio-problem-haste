{-# LANGUAGE OverloadedStrings #-}
module Radio.Field where

import Haste hiding (style)
import Haste.Graphics.Canvas
import Haste.Perch hiding (head)
import Haste.HPlay.View hiding (head)
import Prelude hiding (id, div)
import Control.Monad.IO.Class
import Data.Foldable (find)

import Radio.Grid
import Radio.Tower 
import Radio.Task 
import Radio.Util

-- | active button. When clicked, return the first parameter
cbutton :: a -> String -> Widget a
cbutton x slabel= static $ do
        button slabel ! id slabel ! atr "class" "btn btn-primary" ! atr "type" "button" `pass` OnClick
        return x
      `continuePerch` slabel

fieldConfigWidget :: Input -> Double -> Widget Input
fieldConfigWidget input cellSize = do
  --writeLog $ show $ inputTowers input
  (div ! atr "class" "row vertical-align" <<<
    (   div ! atr "class" "col-md-6" <<< editingCntl
    <|> div ! atr "class" "col-md-6" <<< field ))
    `wcallback` (\newInput -> fieldConfigWidget newInput cellSize)
  where
    field = fieldConfig input cellSize 

    bsrow = div ! atr "class" "row"

    editingCntl :: Widget Input
    editingCntl = bsrow <<<
          (div ! atr "class" "col-md-8" <<< radiusCntl <|> evolOptionsCnt) 
      <|> (div ! atr "class" "col-md-4" <<< fitnessCntl)

    radiusCntl :: Widget Input
    radiusCntl = do
      let f = inputInt (Just $ inputRadius input) ! atr "size" "2"
              `fire` OnKeyUp
          incBtn = cbutton (inputRadius input + 1) "+" `fire` OnClick
          decBtn = cbutton (inputRadius input - 1) "-" `fire` OnClick
      newRadius <- bsrow ! atr "style" "margin-bottom: 40px" <<<
          (div ! atr "class" "col-md-12" <<< (label ("Радиус: " :: JSString) ++> incBtn <|> f <|> decBtn) )
          `validate` (\r -> return $ if r > 0 then Nothing else Just $ b ("радиус отрицателен" :: JSString))
      return $ input {
        inputRadius = newRadius
      }

    fitnessCntl :: Widget Input
    fitnessCntl = do
      newFitness <- 
        label ("Фитнес функция: " :: JSString) ++>
          textArea (inputFitness input) ! atr "rows" "6" ! atr "cols" "60" <++ br 
          <** inputSubmit "Обновить" `fire` OnClick
      return $ input {
        inputFitness = newFitness
      }

    evolOptionsCnt :: Widget Input 
    evolOptionsCnt = do
      newOptions <- bsrow <<< (label ("Настройки эволюции:" :: JSString) ++> evolOptionsCnt')
      liftIO $ writeLog $ show newOptions
      return $ input {
        inputEvolOptions = newOptions
      }
      where
        options = inputEvolOptions input

        evolOptionsCnt' :: Widget EvolOptions 
        evolOptionsCnt' = EvolOptions <$> mutChanceCnt <*> elitePartCnt <*> maxGenCnt <*> popCountCnt <*> indCountCnt
          <** inputSubmit "Обновить" `fire` OnClick

        mutChanceCnt :: Widget Float
        mutChanceCnt = bsrow <<< (
          (div ! atr "class" "col-md-6" $ label ("Шанс мутации: " :: JSString)) ++>
          (div ! atr "class" "col-md-6" <<< inputFloat (Just $ mutationChance options)
          `validate`
          (\c -> return $ if c >= 0.0 && c <= 1.0 then Nothing else Just $ b ("вероятность некорректна [0, 1]" :: JSString))))

        elitePartCnt :: Widget Float
        elitePartCnt = bsrow <<< (
          (div ! atr "class" "col-md-6" $ label ("Часть элиты: " :: JSString)) ++>
          (div ! atr "class" "col-md-6" <<< inputFloat (Just $ elitePart options)
          `validate`
          (\c -> return $ if c >= 0.0 && c <= 1.0 then Nothing else Just $ b ("доля некорректна [0, 1]" :: JSString))))

        maxGenCnt :: Widget Int
        maxGenCnt = bsrow <<< (
          (div ! atr "class" "col-md-6" $ label ("Макс поколений: " :: JSString)) ++>
          (div ! atr "class" "col-md-6" <<< inputInt (Just $ maxGeneration options)
          `validate`
          (\c -> return $ if c > 0 then Nothing else Just $ b ("должно быть положительно" :: JSString))))

        popCountCnt :: Widget Int
        popCountCnt = bsrow <<< (
         (div ! atr "class" "col-md-6" $ label ("Число популяций: " :: JSString)) ++>
         (div ! atr "class" "col-md-6" <<< inputInt (Just $ popCount options)
         `validate`
         (\c -> return $ if c > 0 then Nothing else Just $ b ("должно быть положительно" :: JSString))))

        indCountCnt :: Widget Int
        indCountCnt = bsrow <<< (
         (div ! atr "class" "col-md-6" $ label ("Число индивидов в популяции: " :: JSString)) ++>
         (div ! atr "class" "col-md-6" <<< inputInt (Just $ indCount options)
         `validate`
         (\c -> return $ if c > 0 then Nothing else Just $ b ("должно быть положительно" :: JSString))))

fieldConfig :: Input -> Double -> Widget Input
fieldConfig input cellSize = do
  let g = grid xsize ysize cellSize -- render grid
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
        translate (cellSize/2, cellSize/2) $ stroke $ circle (0, 0) (cellSize * (0.5 + fromIntegral (towerRadius t)))
      placeTower t = translate (fst gridOffset + fromIntegral (towerX t) * cellSize
                              , snd gridOffset + fromIntegral (towerY t) * cellSize)
      drawTowers = mapM_ (\t -> placeTower t $ drawTower t) towers

  canvasId <- fmap ("canvas" ++) getNextId
  resetEventData
  wraw (do
    canvas ! id canvasId
        -- ! style "border: 1px solid black;" 
           ! atr "width" (show viewWidth)
           ! height (show viewHeight)
            $ noHtml)
    `fire` OnClick
  
  wraw $ liftIO $ do 
    wcan <- getCanvasById canvasId
    case wcan of 
      Nothing -> return ()
      Just can -> render can $ do
        translate gridOffset $ drawGrid g
        xlabels
        ylabels
        drawTowers

  e@(EventData typ _) <- getEventData
  evdata <- continueIf (evtName OnClick == typ) e

  offset <- liftIO $ getElementPosition $ "#" ++ canvasId
  -- alert $ show offset 

  mousePos <- liftIO getMousePosition
  let cell = toCell offset g mousePos
  -- alert $ show $ evData evdata
  -- writeLog $ "Cell " ++ show cell ++ " inbounds: " ++ show (inBounds cell)

  let newTowers = if inBounds cell then updateTowers cell else towers
  return $ input { inputTowers = newTowers }
  where
    (xsize, ysize) = inputFieldSize input
    towers = inputTowers input

    toCell :: (Int, Int) -> Grid -> (Int, Int) -> (Int, Int)
    toCell (ofx, ofy) g (mx, my) = (cx -1, cy -1)
      where (cx, cy) = pixelToCell g (mx-ofx, my-ofy)

    inBounds :: (Int, Int) -> Bool
    inBounds (cx, cy) = cx >= 0 && cy >= 0 && cx < xsize && cy < ysize

    updateTowers :: (Int, Int) -> [Tower]
    updateTowers (x, y) = 
      case mt of
        Just t -> filter (\t -> towerX t /= x || towerY t /= y) towers
        Nothing -> Tower x y (inputRadius input) : towers 
      where
        mt = find (\t -> towerX t == x && towerY t == y) towers

