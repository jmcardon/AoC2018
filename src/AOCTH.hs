{-# LANGUAGE TemplateHaskell #-}

module AOCTH where

import           Language.Haskell.TH

-- Template haskell because why not
allExercises :: Int -> ExpQ
allExercises i =
  pure $ ListE $ (\a -> VarE (mkName ("runExercise" <> show a))) <$> [1 .. i]
