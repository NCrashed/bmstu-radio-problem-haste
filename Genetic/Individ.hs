{-# LANGUAGE TypeFamilies #-}
module Genetic.Individ where

import Control.Monad.Random
import Control.Monad.Coroutine
import Control.Monad.Coroutine.SuspensionFunctors

import Genetic.Coroutine

class Individ a where
  type IndividOptions a :: *
  crossover :: IndividOptions a -> a -> a -> PauseableRand (a, a)
  mutation :: IndividOptions a -> a -> PauseableRand a
  initIndivid :: IndividOptions a -> PauseableRand a 