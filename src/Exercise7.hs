{-# LANGUAGE TypeFamilies #-}

module Exercise7
  ( runExercise7
  )
where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Text.Megaparsec                ( parseMaybe
                                                , Parsec
                                                )
import           Text.Megaparsec.Char           ( letterChar
                                                , string
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.List                      ( sort
                                                , nub
                                                , uncons
                                                , partition
                                                , sortOn
                                                )
import           Data.Char                      ( ord )

runExercise7 :: FilePath -> IO ()
runExercise7 fp = do
  l <- lines <$> readFile fp
  let p        = traverse (parseMaybe parseStep) l
      steps    = maybe (error "parseError") id p
      orig     = fst <$> steps
      stepMap  = nub <$> foldl' combineStep Map.empty steps
      stepSeq  = stepSequence orig stepMap
      consumed = stepSequence2 5 orig stepMap
  putStrLn $ "The original sequence is: \"" <> stepSeq <> "\""
  putStrLn $ "The total time consumed is " <> show consumed

type Step = (Char, Char)

parseStep :: Parsec () String Step
parseStep = do
  string "Step "
  l <- letterChar
  string " must be finished before step "
  r <- letterChar
  string " can begin."
  pure (l, r)

duration :: Char -> Int
duration c = ord c - 4

combineStep :: Map Char [Char] -> Step -> Map Char [Char]
combineStep m (prec, dep) = Map.insertWith (++) dep [prec] m

stepSequence :: [Char] -> Map Char [Char] -> [Char]
stepSequence origList mm =
  let orig          = sort $ nub origList
      elems         = Map.keys mm
      unc           = uncons (filter (flip notElem elems) orig)
      (first, rest) = maybe (error "no root element") id unc
  in  combineSeq first rest [] mm
 where
  dub []       f s _ = reverse (f : s)
  dub (x : xs) f s m = combineSeq x xs (f : s) m
  combineSeq f tp s m =
    -- remove f from all dependencies
    let removedDep    = filter (/= f) <$> m
        -- return a map of the available elems now.
        (avail, rest) = Map.partition null removedDep
        -- return a list of all of the possible elements to popnow.
        availList     = nub $ sort $ Map.keys avail ++ tp
    in  dub availList f s rest

stepSequence2 :: Int -> [Char] -> Map Char [Char] -> Int
stepSequence2 parallel origList mm =
  let orig          = sort $ nub origList
      elems         = Map.keys mm
      (roots, rest) = splitAt parallel $ filter (flip notElem elems) orig
  in  combineSeq (sortOn snd $ withTime <$> roots) rest mm 0
 where
  withTime c = (c, duration c)
  decrement dec (c, dur) = (c, dur - dec)
  isDone (_, d) = d <= 0
  -- On a _sorted_ list of durations, tick the smallest one.
  tickMin []             = ([], 0)
  tickMin l@((_, d) : _) = (decrement d <$> l, d)
  -- remove the entry from the map
  filterEntry m (d, _) = filter (/= d) <$> m
  combineSeq [] [] _ acc = acc
  combineSeq roots rest mm acc =
    let (t      , elapsed  )        = tickMin roots
        (removed, remaining)        = partition isDone t
        filtered                    = foldl' filterEntry mm removed
        capacity                    = parallel - (length remaining)
        (newRoots, newRest, newmap) = addFreed remaining capacity filtered rest
    in  combineSeq (sortOn snd newRoots) newRest newmap (acc + elapsed)
  addFreed remaining capacity rawMap ready =
    let (available, enqueued) = Map.partition null rawMap
        (toAppend, rest) =
          splitAt capacity $ sort (Map.keys available ++ ready)
        newRemaining = remaining ++ (withTime <$> toAppend)
    in  (newRemaining, rest, enqueued)
