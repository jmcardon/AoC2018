{-# LANGUAGE TemplateHaskell #-}

module AOCTH where

import           Language.Haskell.TH

-- Template haskell because why not
allExercises :: Int -> ExpQ
allExercises i =
  pure $ ListE [ VarE (mkName ("runExercise" <> show x)) | x <- [1 .. i] ]
