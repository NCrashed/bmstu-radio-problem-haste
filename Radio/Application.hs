{-# LANGUAGE OverloadedStrings #-}
module Radio.Application where

import Prelude hiding (div)
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Radio.Field
import Radio.Task
import Radio.Util
import Radio.Genetic
import Radio.Plot
import Radio.Config
import Radio.Tower
import System.Random
import Haste.HPlay.View hiding (head)
import Haste

data ApplicationState = AppConfigure Input 
  | AppCalculate Input PlotState GeneticState
  | AppShow Input PlotState Output
  deriving (Show)

data Route = RouteConfig | RouteCalculate | RouteShow
  deriving (Enum, Show)

initialState :: ApplicationState
initialState = AppConfigure initialInput

runApplication :: ApplicationState -> Widget ()
runApplication state = wloop state go
  where 
    go :: ApplicationState -> Widget ApplicationState
    go localState@(AppConfigure input) = do
      update <- eitherWidget (fieldConfigWidget input 50) $ routeWidget localState
      case update of
        Right route -> case route of 
          RouteCalculate -> do
            geneticState <- liftIO initialGeneticState
            return $ AppCalculate input initialPlotState geneticState
          _ -> fail $ "invalid route in config state " ++ show route
        Left newInput -> return $ AppConfigure newInput

    go localState@(AppCalculate input plotState geneticState) = do
      update <- eitherWidget (geneticWidget input geneticState plotState) $ routeWidget localState
      case update of 
        Right route -> case route of 
          RouteConfig -> return $ AppConfigure input 
          RouteShow -> return $ AppShow input plotState $ extractSolution input geneticState
          _ -> fail $ "invalid route in config state " ++ show route
        Left (newGeneticState, newPlotState) -> return $ if isGeneticFinished newGeneticState 
          then AppShow input newPlotState $ extractSolution input newGeneticState
          else AppCalculate input newPlotState newGeneticState

    go localState@(AppShow input plotState output) = do
      update <- eitherWidget (showResultsWidget input plotState output) $ routeWidget localState
      case update of
        Right route -> case route of 
          RouteConfig -> return $ AppConfigure input 
          RouteCalculate -> do 
            geneticState <- liftIO initialGeneticState
            return $ AppCalculate input initialPlotState geneticState 
          _ -> fail $ "invalid route in show state " ++ show route
        Left _ -> return localState

eitherWidget :: Widget a -> Widget b -> Widget (Either a b)
eitherWidget wa wb = (return . Left =<< wa) <|> (return . Right =<< wb)

routeWidget :: ApplicationState -> Widget Route
routeWidget state = div ! atr "class" "row" 
  <<< div ! atr "class" "col-md-2 col-md-offset-5"
  <<< go state
  where
    go (AppConfigure _) = bigBtn RouteCalculate "Начать эволюцию"
    go (AppCalculate _ _ _) = bigBtn RouteConfig "Назад"  <|> bigBtn RouteShow "Остановить"
    go (AppShow _ _ _) = bigBtn RouteConfig "Начать с начала" <|> bigBtn RouteCalculate "Перерасчитать"

    bigBtn v s = cbutton v s <! [atr "class" "btn btn-primary btn-lg"]

geneticWidget :: Input -> GeneticState -> PlotState -> Widget (GeneticState, PlotState)
geneticWidget input geneticState plotState = do 
  --wprint $ show $ geneticCurrentBest geneticState

  let newPlotState =  if null $ geneticPopulations geneticState
                      then plotState
                      else plotState 
                      { 
                        values = values plotState ++ [
                          ( geneticCurrentGen geneticState, 
                            fst $ geneticCurrentBest geneticState
                          )] 
                      }

  div ! atr "class" "col-md-2 col-md-offset-3" <<< plotWidget newPlotState "Поколение" "Фитнес" (900, 500)
  newGeneticState <- timeout 200 $ liftIO $ solve input geneticState
  return (newGeneticState, newPlotState)

showResultsWidget :: Input -> PlotState -> Output -> Widget ()
showResultsWidget input plotState output = div ! atr "class" "row" <<< do
  div ! atr "class" "col-md-6" <<< fieldShow input output 50 
  div ! atr "class" "col-md-6" <<< do
    plotWidget plotState "Поколение" "Фитнес" (900, 500)
    wraw $ div ! atr "class" "row-fluid" $ mconcat [
        div ! atr "class" "col-md-6" $ inputInfo
      , div ! atr "class" "col-md-6" $ optionsInfo
      , div ! atr "class" "col-md-6" $ outputInfo
      ]
  noWidget
  where
    opts = inputEvolOptions input 
    showTower t = "x: " ++ show (towerX t) ++ " y: " ++ show (towerY t) ++ " r: " ++ show (towerRadius t)
    showTower' t = "x: " ++ show (towerX t) ++ " y: " ++ show (towerY t)

    inputInfo = panel "Входные данные" $ mconcat [
        labelRow "Размер поля:" $ show $ inputFieldSize input
      , labelRow "Возможные башни:" $ show $ showTower <$> inputTowers input
      ]

    optionsInfo = panel "Настройки эволюции" $ mconcat [
        labelRow "Шанс мутации: " $ show $ mutationChance opts
      , labelRow "Часть элиты: " $ show $ elitePart opts
      , labelRow "Максимальное число поколений: " $ show $ maxGeneration opts
      , labelRow "Кол-во популяций: " $ show $ popCount opts
      , labelRow "Кол-во индивидов в популяции: " $ show $ indCount opts
      ]

    outputInfo = panel "Результаты эволюции" $ mconcat [
        labelRow "Лучший фитнес: " $ show $ outputFitness output
      , labelRow "Лучшее решение: " $ show $ showTower' <$> outputTowers output
      ]

    panel :: String -> Perch -> Perch
    panel ts bd = div ! atr "class" "panel panel-default" $ mconcat [
        div ! atr "class" "panel-heading" $ h3 ! atr "class" "panel-title" $ toJSString ts
      , div ! atr "class" "panel-body" $ bd
      ]

    labelRow :: String -> String -> Perch
    labelRow ts vs = div ! atr "class" "row" $ mconcat [
        div ! atr "class" "col-md-4" $ label $ toJSString ts
      , div ! atr "class" "col-md-8" $ toJSString vs
      ]