module Exercise6
  ( runExercise6
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Text.Megaparsec                ( parseMaybe
                                                , Parsec
                                                , single
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )
import           Text.Megaparsec.Char           ( space1 )

import           Control.Monad.State.Strict     ( runState
                                                , StateT(..)
                                                )
import           Data.Foldable                  ( foldl' )
import           Data.List                      ( sortOn
                                                , nub
                                                )
import           Data.Maybe                     ( catMaybes )

type Point a = (a, a)

type Grid = [Point Int]

newtype TaggedPoint a = TaggedPt {
    pt :: Point a
} deriving (Eq, Show, Ord)

data Bound = Bound {
    xBound :: Int,
    yBound :: Int
} deriving (Eq, Show)

parsePoint :: Parsec () String (Point Int)
parsePoint = do
  i <- decimal
  single ','
  space1
  (,) i <$> decimal

calcGrid
  :: [TaggedPoint Int] -> Bound -> (Grid, Map (Point Int) (TaggedPoint Int))
calcGrid pts (Bound bx by) =
  let grid      = [ (x, y) | x <- [0 .. bx], y <- [0 .. by] ]
      gridMap   = foldl' (mkMap pts) Map.empty grid
      borderSet = nub $ borders gridMap
  in  (grid, Map.filter (flip notElem borderSet) gridMap)
 where
  borders m =
    let topBorder    = [ (x, 0) | x <- [0 .. bx] ]
        leftBorder   = [ (0, y) | y <- [0 .. by] ]
        bottomBorder = [ (x, by) | x <- [0 .. bx] ]
        rightBorder  = [ (bx, y) | y <- [0 .. by] ]
    in  catMaybes
          [ Map.lookup x m
          | x <- topBorder ++ leftBorder ++ bottomBorder ++ rightBorder
          ]
  mkMap pts m (x, y) =
    let elems = sortOn (absDistance x y) pts in checkEqui x y m elems
  checkEqui x y m (n1 : n2 : _) | absDistance x y n1 == absDistance x y n2 = m
                                | otherwise = Map.insert (x, y) n1 m
  checkEqui _ _ _ _ = error "fatal"

mkPoints :: [String] -> ([TaggedPoint Int], Bound)
mkPoints s = runState (traverse pst s) (Bound 0 0)
 where
  pst inp =
    withState $ maybe (error "parseError") id (parseMaybe parsePoint inp)
  withState (x, y) = StateT $ \s -> pure (TaggedPt (x, y), mkBound x y s)
  mkBound x y (Bound bx by) | x > bx && y > by = Bound x y
                            | x > bx           = Bound x by
                            | y > by           = Bound bx y
                            | otherwise        = Bound bx by

mkCountMap
  :: Ord a
  => Map (TaggedPoint a) Int
  -> (Point a, TaggedPoint a)
  -> Map (TaggedPoint a) Int
mkCountMap m (_, namedpt) = Map.insertWith (+) namedpt 1 m

absDistance :: Num a => a -> a -> TaggedPoint a -> a
absDistance x y (TaggedPt (nx, ny)) = abs (nx - x) + abs (ny - y)

regionLT :: Int -> Grid -> [TaggedPoint Int] -> Grid
regionLT i grid n = filter (regionLT' n i) grid
  where regionLT' n i (x, y) = sum (absDistance x y <$> n) < i

runExercise6 :: FilePath -> IO ()
runExercise6 s = do
  l <- lines <$> readFile s
  let (rawPts , bound) = mkPoints l
      (rawGrid, grid ) = calcGrid rawPts bound
      countMap         = foldl' mkCountMap Map.empty (Map.toList grid)
      maxArea          = head $ sortOn (negate . snd) (Map.toList countMap)
  putStr $ "The points with the maximum area was: "
  print maxArea
  putStr "The area of the region less than 10k is: "
  print $ length $ regionLT 10000 rawGrid rawPts
