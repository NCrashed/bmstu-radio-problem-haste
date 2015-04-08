{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Radio.Task where

import Radio.Tower 
import Haste.Serialize
import Haste.JSON
import Data.Typeable
import Data.Maybe
import Control.Applicative

data Input = Input {
  inputFieldSize :: (Int, Int),
  inputTowers :: [Tower],
  inputRadius :: Int,
  inputFitness :: String, -- ^ JS expression
  inputEvolOptions :: EvolOptions
} deriving (Typeable, Show)

instance Serialize Input where
  toJSON i = Dict [
      ("inputFieldSize", toJSON $ inputFieldSize i)
    , ("inputTowers", toJSON $ inputTowers i)
    , ("inputRadius", toJSON $ inputRadius i)
    , ("inputFitness", toJSON $ inputFitness i)
    , ("inputEvolOptions", toJSON $ inputEvolOptions i)
    ] 
  parseJSON j = Input 
    <$> j .: "inputFieldSize"
    <*> j .: "inputTowers"
    <*> j .: "inputRadius"
    <*> j .: "inputFitness"
    <*> j .: "inputEvolOptions"

initialInput :: Input
initialInput = Input {
    inputFieldSize = (20, 20),
    inputTowers = [],
    inputRadius = 3,
    inputFitness = "function(coverage, usedCount, totalCount)\n{\n    return coverage*(1 - usedCount / totalCount);\n}",
    inputEvolOptions = initialOptions
  }

data Output = Output {
  outputTowers :: [Tower],
  outputFitness :: Float
} deriving (Typeable, Show)

data EvolOptions = EvolOptions {
  mutationChance :: Float,
  elitePart :: Float,
  maxGeneration :: Int,
  popCount :: Int,
  indCount :: Int,
  targetFitness :: Maybe Float
} deriving (Typeable, Show)

instance Serialize EvolOptions where
  toJSON o = Dict $ [
      ("mutationChance", toJSON $ mutationChance o)
    , ("elitePart", toJSON $ elitePart o)
    , ("maxGeneration", toJSON $ maxGeneration o)
    , ("popCount", toJSON $ popCount o)
    , ("indCount", toJSON $ indCount o)
    ] ++ if isJust $ targetFitness o 
      then [("targetFitness", toJSON $ fromJust $ targetFitness o)]
      else []

  parseJSON j = EvolOptions
    <$> j .: "mutationChance"
    <*> j .: "elitePart"
    <*> j .: "maxGeneration"
    <*> j .: "popCount"
    <*> j .: "indCount"
    <*> j .:? "targetFitness"

initialOptions :: EvolOptions
initialOptions = EvolOptions {
    mutationChance = 0.3,
    elitePart = 0.1,
    maxGeneration = 100,
    popCount = 1,
    indCount = 10,
    targetFitness = Nothing
  }

data PlotState = PlotState{
  values :: [(Int, Float)] -- ^ Points: x - generation number, y - fitness value
} deriving (Typeable, Show)

initialPlotState :: PlotState 
initialPlotState = PlotState []