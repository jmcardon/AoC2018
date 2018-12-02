module Main where

import           Exercise1                      ( printFreq )
import           Exercise2                      ( runCount )

main :: IO ()
main = do
  printFreq "input_problem1.txt"
  runCount "input_problem2.txt"


