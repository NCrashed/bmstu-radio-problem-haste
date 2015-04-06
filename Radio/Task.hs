{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Radio.Task where

import Radio.Tower 
import Haste.Serialize
import Haste.JSON
import Data.Typeable
import Control.Applicative

data Input = Input {
  inputFieldSize :: (Int, Int),
  inputTowers :: [Tower],
  inputRadius :: Int,
  inputFitness :: String, -- ^ JS expression
  inputEvolOptions :: EvolOptions
} deriving (Typeable)

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
    inputFieldSize = (10, 10),
    inputTowers = [],
    inputRadius = 2,
    inputFitness = "function(coverage, usedCount, totalCount)\n{\n    return coverage*(1 - usedCount / totalCount);\n}",
    inputEvolOptions = initialOptions
  }

data Output = Output {
  outputTowers :: [Tower],
  outputFitness :: Float
}

data EvolOptions = EvolOptions {
  mutationChance :: Float,
  elitePart :: Float,
  maxGeneration :: Int,
  popCount :: Int,
  indCount :: Int
} deriving (Typeable, Show)

instance Serialize EvolOptions where
  toJSON o = Dict [
      ("mutationChance", toJSON $ mutationChance o)
    , ("elitePart", toJSON $ elitePart o)
    , ("maxGeneration", toJSON $ maxGeneration o)
    , ("popCount", toJSON $ popCount o)
    , ("indCount", toJSON $ indCount o)
    ]
  parseJSON j = EvolOptions
    <$> j .: "mutationChance"
    <*> j .: "elitePart"
    <*> j .: "maxGeneration"
    <*> j .: "popCount"
    <*> j .: "indCount"

initialOptions :: EvolOptions
initialOptions = EvolOptions {
    mutationChance = 0.3,
    elitePart = 0.1,
    maxGeneration = 10,
    popCount = 1,
    indCount = 10 
  }

data PlotState = PlotState{
  values :: [(Int, Float)] -- ^ Points: x - generation number, y - fitness value
}

initialPlotState :: PlotState 
initialPlotState = PlotState []