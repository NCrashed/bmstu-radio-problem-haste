module Main where

import Haste
import Haste.HPlay.View hiding (head)

import Radio.Field
import Radio.Task
import Control.Monad.IO.Class

main :: IO (Maybe ())
main = runBody $ center <<< (fieldConfigWidget initialInput 50 >> return ())
