{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Exercise4
  ( runExercise4
  )
where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Foldable                  ( foldl'
                                                , foldlM
                                                )
import           Text.Megaparsec                ( parseMaybe
                                                , Parsec
                                                , single
                                                , try
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )
import           Text.Megaparsec.Char           ( space1
                                                , string
                                                )
import           Control.Monad.State.Strict     ( MonadState(..)
                                                , evalState
                                                )
import           Control.Applicative            ( (<|>) )
import           Data.List                      ( sortOn )


data GuardStatus = Begin Int | FallAsleep Int | WakeUp Int deriving (Eq, Show)

-- A simple data structure representing our date
-- values. 
data Time = Time {
    year :: Int,
    month :: Int,
    day :: Int,
    hr :: Int,
    minutes :: Int
} deriving (Eq, Show, Ord)

-- Get an absolute representation of the time, in minutes,
-- imagining year 0 as being the first possible year.
absTime :: Time -> Int
absTime t1 = yearTime + monthTime + dayTime + hrTime + minutesTime
 where
  yearTime    = year t1 * 525600
  monthTime   = month t1 * 43800
  dayTime     = day t1 * 1440
  hrTime      = hr t1 * 60
  minutesTime = minutes t1

data Info = Info {
    guardStatus :: GuardStatus,
    time :: Time
 } deriving (Eq, Show)

data GuardMinute = GuardMinute {
    gminId :: Int,
    gminMinute :: Int
 } deriving (Eq, Ord)

instance Show GuardMinute where
    show (GuardMinute i m) = "[Guard #" <> show i <> " at minute " <> show m <> "]"

prodGM :: GuardMinute -> Int
prodGM g = gminId g * gminMinute g

fixIndices :: MonadState Int m => Info -> m Info
fixIndices (Info g t) = case g of
  (Begin      i) -> put i *> pure (Info g t)
  (FallAsleep _) -> flip Info t . FallAsleep <$> get
  (WakeUp     _) -> flip Info t . WakeUp <$> get

calcSleepTime
  :: MonadState Info m => Map GuardMinute Int -> Info -> m (Map GuardMinute Int)
calcSleepTime m inf@(Info g t2) = onG g
 where
  onG (Begin      _) = put inf *> pure m
  onG (FallAsleep _) = do
    (Info gg _) <- get
    case gg of
      (FallAsleep _) -> error "fatal: Cannot fall asleep twice in a row"
      (WakeUp     _) -> put inf *> pure m
      (Begin      _) -> put inf *> pure m
  onG (WakeUp i) = do
    (Info gg t1) <- get
    case gg of
      (FallAsleep _) -> onWakeup t1 t2 i
      (WakeUp     _) -> error "fatal: Cannot wake up twice in a row"
      (Begin      _) -> error "fatal: Cannot wake up before having slept"
  onWakeup t1 t2 i = do
    put inf
    let
      total      = absTime t2 - absTime t1
      lowerbound = minutes t1
      li =
        [ GuardMinute i (x `mod` 60)
        | x <- [lowerbound .. lowerbound + total - 1]
        ]
    pure $ foldl' increment m li
  increment m i = Map.insertWith (+) i 1 m

parseInfo :: Parsec () String Info
parseInfo = do
  single '['
  c <- getTime
  single ']'
  space1
  info <- try begin <|> try fallAsleep <|> wakeUp
  pure $ Info info c
 where
  getTime = do
    y <- decimal
    single '-'
    m <- decimal
    single '-'
    d <- decimal
    space1
    hr <- decimal
    single ':'
    min <- decimal
    pure $ Time y m d hr min
  begin = do
    string "Guard #"
    i <- decimal
    space1
    string "begins shift"
    pure $ Begin i
  fallAsleep = do
    string "falls asleep"
    pure $ FallAsleep 0
  wakeUp = do
    string "wakes up"
    pure $ WakeUp 0

intoMap2 :: Map Int Integer -> (GuardMinute, Int) -> Map Int Integer
intoMap2 m ((GuardMinute gid _), amt) =
  Map.insertWith (+) gid (fromIntegral amt) m

runExercise4 :: FilePath -> IO ()
runExercise4 fp = do
  li <- lines <$> readFile fp
  let
    rawSorted = sortOn (absTime . time)
      $ maybe (error "parseError") id (traverse (parseMaybe parseInfo) li)
    -- Extract the parsed data
    rawData = evalState (traverse fixIndices rawSorted) (-1)
    -- A map of (guardId, minute) -> total slept
    sleepPerMin =
      evalState (foldlM calcSleepTime Map.empty rawData) (head rawData)
    -- The tuple representation of (guardId, minute, total slept)
    tsElems   = Map.toList sleepPerMin
    -- A map of total time slept per guard
    tsMap     = foldl' intoMap2 Map.empty tsElems
    -- The guard id with the maximum amount of time slept
    -- by sorting by the max time slept (Descending) and
    -- getting the top element
    gidMax    = fst . head $ sortOn ((*) (-1) . snd) (Map.toList tsMap)
    guardMin1 = fst . head $ sortOn ((*) (-1) . snd) $ filter
      ((==) gidMax . gminId . fst)
      tsElems
    guardMin2 = fst . head $ reverse $ sortOn snd tsElems
  putStrLn $ "The guard who slept the most: " <> (show guardMin1)
  putStrLn $ "The product of the first guard: " <> show (prodGM guardMin1)
  putStrLn
    $  "The guard who had the most minutes slept on a singular minute: "
    <> show guardMin2
  putStrLn $ "The product of the second guard: " <> (show . prodGM) guardMin2

