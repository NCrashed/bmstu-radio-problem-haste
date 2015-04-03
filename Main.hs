module Main where

import Haste.HPlay.View hiding (head)

import Radio.Field
import Radio.Tower 

main :: IO (Maybe ())
main = runBody $ fieldConfig (10, 10) 50 [Tower 1 1 2]