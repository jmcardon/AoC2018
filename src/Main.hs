{-# LANGUAGE TemplateHaskell #-}

module Main where

import           AOCTH
import           Data.Foldable                  ( traverse_ )
import           Exercise1                      ( runExercise1 )
import           Exercise2                      ( runExercise2 )
import           Exercise3                      ( runExercise3 )
import           Exercise4                      ( runExercise4 )
import           Exercise5                      ( runExercise5 )
import           Options.Applicative            ( Parser
                                                , option
                                                , auto
                                                , switch
                                                , long
                                                , ParserInfo
                                                , helper
                                                , fullDesc
                                                , progDesc
                                                , (<**>)
                                                , header
                                                , info
                                                , (<|>)
                                                , execParser
                                                )

data ToRun = Single Int | Range Int Int | All deriving (Show)

single :: Parser ToRun
single = Single <$> option auto (long "single")

range :: Parser ToRun
range = Range <$> option auto (long "start") <*> option auto (long "end")

allT :: Parser ToRun
allT = switch (long "all") *> pure All

parseOptions :: ParserInfo ToRun
parseOptions =
  let parseAll = single <|> range <|> allT
  in  info
        (parseAll <**> helper)
        (fullDesc <> progDesc "Run all of the advent of code problems" <> header
          "Advent of code 2018 cli"
        )

main :: IO ()
main = do
  let s = exercises
  runOption s =<< execParser parseOptions
 where
  exercises = $(allExercises 5)
  invalidRange i = "Invalid exercise range! must be within 1 and " <> show i
  problemInput i = "input_problem" <> show i <> ".txt"
  validRange s i = i > 0 && i <= length s
  runProblem s i = do
    putStrLn ("Running Exercise " <> show i)
    (s !! (i - 1)) (problemInput i)
  runOption s (Single i) | validRange s i = runProblem s i
                         | otherwise      = putStrLn $ invalidRange (length s)
  runOption s (Range lower upper)
    | validRange s lower && validRange s upper && lower <= upper = traverse_
      (runProblem s)
      [lower .. upper]
    | otherwise = putStrLn $ invalidRange (length s)
  runOption s All = traverse_ (runProblem s) [1 .. length s]
