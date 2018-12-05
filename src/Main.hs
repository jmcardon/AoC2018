module Main where

import           Exercise1                      ( printFreq )
import           Exercise2                      ( runCount )
import           Exercise3                      ( runAllClaims )
import           Exercise4                      ( runExercise4 )
import           Exercise5                      ( runExercise5 )

main :: IO ()
main = do
  printFreq "input_problem1.txt"
  runCount "input_problem2.txt"
  runAllClaims "input_problem3.txt"
  runExercise4 "input_problem4.txt"
  runExercise5 "input_problem5.txt"


