{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Exercise2
  ( runExercise2
  )
where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as M
import           Data.List                      ( find )

fkedEq :: Eq a => [a] -> [a] -> Bool
fkedEq l r = fkedEq' l r False
 where
  fkedEq' [] [] f = f
  fkedEq' (x : xs) (y : ys) f | f && x /= y       = False
                              | (not f) && x /= y = fkedEq' xs ys True
                              | x == y            = fkedEq' xs ys f
  fkedEq' _ _ _ = False

freqMap :: Ord a => [a] -> Map a Int
freqMap s = M.fromListWith (+) [ (x, 1) | x <- s ]

checkSum :: Ord a => [[a]] -> Int
checkSum li = twos * threes
 where
  twos   = countN 2
  threes = countN 3
  countN n = length $ filter (elem n) (freqMap <$> li)

runExercise2 :: FilePath -> IO ()
runExercise2 f = do
  l <- lines <$> readFile f
  let cs             = checkSum l
      (lword, rword) = matchingTwo l
      common         = filter (\s -> elem s rword) lword
  putStrLn $ "The checksum is: " <> (show cs)
  putStrLn $ "The common letters are " <> common

matchingTwo :: Eq a => [[a]] -> ([a], [a])
matchingTwo li = throughAll li []
 where
  throughAll []       _   = error "YOU LIED TO ME AOC"
  throughAll (x : xs) acc = case find (fkedEq x) acc of
    Just y  -> (x, y)
    Nothing -> throughAll xs (x : acc)
