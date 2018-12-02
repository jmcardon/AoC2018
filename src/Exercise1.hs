{-# LANGUAGE BangPatterns #-}

module Exercise1
  ( printFreq
  , freqList
  )
where

import           Text.Read                      ( readMaybe )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Debug.Trace                    ( trace )

freqList :: FilePath -> IO [Int]
freqList f = do
  l <- lines <$> readFile f
  let m = traverse (readMaybe . filter (/= '+')) l
  maybe (fail $ "Error parsing numerics frequencies in file: " <> f) pure m


printFreq :: FilePath -> IO ()
printFreq f = do
  mb <- freqList f
  initialMsg (sum mb)
  repeatedMsg (repeatedFreq mb)
 where
  initialMsg i = putStr "The resulting frequency is " *> print i
  repeatedMsg = putStrLn . (<>) "The repeated frequency was: " . show

repeatedFreq :: [Int] -> Int
repeatedFreq li = recurseLi li 0 (Set.singleton 0)
 where
  recurseLi [] !s set = recurseLi li s set
  recurseLi (!x : xs) !s set =
    let !n = s + x
    in  if Set.member n set then n else recurseLi xs n (Set.insert n set)
