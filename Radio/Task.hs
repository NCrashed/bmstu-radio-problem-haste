{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Radio.Task where

import Radio.Tower 
import Haste.Serialize
import Haste.JSON
import Data.Typeable
import Data.Maybe
import Control.Applicative
import Genetic.Options

data Input = Input {
  inputFieldSize :: (Int, Int),
  inputTowers :: [Tower],
  inputRadius :: Int,
  inputFitness :: String, -- ^ JS expression
  inputGeneticOptions :: GeneticOptions,
  inputRandomField :: (Int, Int, Int)
} deriving (Typeable, Show)

instance (Serialize a, Serialize b, Serialize c) => Serialize (a, b, c) where 
  toJSON (x, y, z) = Dict [
      ("v1", toJSON x)
    , ("v2", toJSON y)
    , ("v3", toJSON z)
    ] 

  parseJSON j = (,,)
    <$> j .: "v1"
    <*> j .: "v2"
    <*> j .: "v3"

instance Serialize Input where
  toJSON i = Dict [
      ("inputFieldSize", toJSON $ inputFieldSize i)
    , ("inputTowers", toJSON $ inputTowers i)
    , ("inputRadius", toJSON $ inputRadius i)
    , ("inputFitness", toJSON $ inputFitness i)
    , ("inputGeneticOptions", toJSON $ inputGeneticOptions i)
    , ("inputRandomField", toJSON $ inputRandomField i)
    ] 
  parseJSON j = Input 
    <$> j .: "inputFieldSize"
    <*> j .: "inputTowers"
    <*> j .: "inputRadius"
    <*> j .: "inputFitness"
    <*> j .: "inputGeneticOptions"
    <*> j .: "inputRandomField"

initialInput :: Input
initialInput = Input {
    inputFieldSize = (20, 20),
    inputTowers = [],
    inputRadius = 3,
    inputFitness = "function(coverage, usedCount, towerUsedGetter, totalCount, towerTotalGetter, fieldWidth, fieldHeight, fieldGetter)\n{\n    return coverage*(1 - usedCount / totalCount);\n}",
    inputGeneticOptions = initialOptions,
    inputRandomField = (10, 2, 3)
  }

data Output = Output {
  outputTowers :: [Tower],
  outputFitness :: Float
} deriving (Typeable, Show)

data PlotState = PlotState{
  values :: [(Int, Float)] -- ^ Points: x - generation number, y - fitness value
} deriving (Typeable, Show)

initialPlotState :: PlotState 
initialPlotState = PlotState []