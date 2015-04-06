module Radio.Application where

import Prelude hiding (div)
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Radio.Field
import Radio.Task
import Radio.Util
import Radio.Genetic
import Radio.Plot
import System.Random
import Haste.HPlay.View hiding (head)

data ApplicationState = AppConfigure Input 
  | AppCalculate Input PlotState GeneticState
  | AppShow Input PlotState Output

data Route = RouteConfig | RouteCalculate | RouteShow
  deriving (Enum, Show)

initialState :: ApplicationState
initialState = AppConfigure initialInput

runApplication :: ApplicationState -> Widget ()
runApplication state = void $ go state `wcallback` runApplication
  where 
    go :: ApplicationState -> Widget ApplicationState
    go (AppConfigure input) = do
      update <- eitherWidget (fieldConfigWidget input 50) $ routeWidget state
      case update of
        Right route -> case route of 
          RouteCalculate -> do
            geneticState <- liftIO initialGeneticState
            return $ AppCalculate input initialPlotState geneticState
          _ -> fail $ "invalid route in config state " ++ show route
        Left newInput -> return $ AppConfigure newInput

    go (AppCalculate input plotState geneticState) = do
      update <- eitherWidget (geneticWidget input geneticState plotState) $ routeWidget state
      case update of 
        Right route -> case route of 
          RouteConfig -> return $ AppConfigure input 
          _ -> fail $ "invalid route in config state " ++ show route
        Left (newGeneticState, newPlotState) -> return $ if isGeneticFinished newGeneticState 
          then AppShow input newPlotState $ extractSolution input newGeneticState
          else AppCalculate input newPlotState newGeneticState

    go (AppShow input plotState output) = fail "show"

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
  wprint $ show geneticState

  let newPlotState =  if null $ geneticPopulations geneticState
                      then plotState
                      else plotState 
                      { 
                        values = values plotState ++ [
                          ( geneticCurrentGen geneticState, 
                            fst $ geneticCurrentBest geneticState
                          )] 
                      }

  plotWidget newPlotState "Поколение" "Фитнес" (800, 400)
  newGeneticState <- cbuttonM (solve input geneticState) "test"
  wprint $ show newGeneticState
  return (newGeneticState, newPlotState)
