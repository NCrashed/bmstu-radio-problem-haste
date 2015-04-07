{-# LANGUAGE OverloadedStrings #-}
module Radio.Config where

import Prelude hiding (id, div)
import Haste hiding (style)
import Haste.Perch hiding (head)
import Haste.HPlay.View hiding (head)

import Radio.Field
import Radio.Task 
import Radio.Util

fieldConfigWidget :: Input -> Double -> Widget Input
fieldConfigWidget input cellSize = do
  --writeLog $ show $ inputTowers input
  div ! atr "class" "row vertical-align" <<<
    (   div ! atr "class" "col-md-6" <<< editingCntl
    <|> div ! atr "class" "col-md-6" <<< field )
  where
    field = fieldConfig input cellSize 

    bsrow = div ! atr "class" "row"

    editingCntl :: Widget Input
    editingCntl = bsrow <<<
          (fieldOptionsCnt <|> evolOptionsCnt <|> fitnessCntl) 
      where
        fieldOptionsCnt = (bsrow $ label ("Настройки поля: " :: JSString) ! atr "style" "font-size: 20px") ++>
          (bsrow <<< (radiusCntl <|> fieldWidthCntl <|> fieldHeightCntl))

    makeCounter :: Int -> JSString -> JSString -> Widget Int
    makeCounter initial labelStr errmsg = bsrow <<< 
      ((div ! atr "class" "col-md-6" $ label (labelStr :: JSString)) ++>
       (div ! atr "class" "col-md-6" <<< (incBtn <|> f <|> decBtn)) ) 
      `validate` (\r -> return $ if r > 0 then Nothing else Just $ b (errmsg :: JSString))  
      where
        f = inputInt (Just initial) ! atr "size" "2" `fire` OnKeyUp
        incBtn = cbutton (initial + 1) "+" `fire` OnClick
        decBtn = cbutton (initial - 1) "-" `fire` OnClick

    radiusCntl :: Widget Input
    radiusCntl = do
      newRadius <- makeCounter (inputRadius input) "Радиус: " "радиус должен быть положителен"
      return $ input {
        inputRadius = newRadius
      }

    fieldWidthCntl :: Widget Input 
    fieldWidthCntl = do
      newWidth <- makeCounter (fst $ inputFieldSize input) "Ширина поля: " "ширина должна быть положительна"
      return $ input {
        inputFieldSize = (newWidth, snd $ inputFieldSize input)
      } 

    fieldHeightCntl :: Widget Input 
    fieldHeightCntl = do
      newHeight <- makeCounter (snd $ inputFieldSize input) "Высота поля: " "высота должна быть положительна"
      return $ input {
        inputFieldSize = (fst $ inputFieldSize input, newHeight)
      } 

    fitnessCntl :: Widget Input
    fitnessCntl = do
      newFitness <- bsrow <<< (
        (bsrow $ label ("Фитнес функция: " :: JSString) ! atr "style" "margin-top: 40px; font-size: 20px") ++>
        (bsrow <<< textArea (inputFitness input) ! atr "rows" "6" ! atr "cols" "60" <++ br 
          <** inputSubmit "Обновить" `fire` OnClick))
      return $ input {
        inputFitness = newFitness
      }

    evolOptionsCnt :: Widget Input 
    evolOptionsCnt = do
      newOptions <- bsrow <<< (label ("Настройки эволюции:" :: JSString) ! atr "style" "margin-top: 40px; font-size: 20px" 
        ++> evolOptionsCnt')
      --liftIO $ writeLog $ show newOptions
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
