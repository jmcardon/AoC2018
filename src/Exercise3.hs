module Exercise3
  ( runExercise3
  )
where

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Foldable                  ( foldl' )
import           Text.Megaparsec                ( parseMaybe
                                                , Parsec
                                                , single
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )
import           Text.Megaparsec.Char           ( space1 )

-- Data representing one individual claim,
-- with accessor functions
-- cId -> get the claim id
-- cXOff -> get the X offset of a particular claim
-- cYOff -> get the Y offset of a particular claim
-- cWidth -> the width of the claim fabric
-- cHeight -> the height of the claim fabrric
data Claim = Claim {
    cId :: Int,
    cXOff :: Int,
    cYOff :: Int,
    cWidth :: Int,
    cHeight:: Int
} deriving (Eq, Show)

-- A simple data structure for
-- Row and column indexing, for clairty
-- RCIx = Row column index
data RCIx = RCIx {
    row :: Int,
    col :: Int
} deriving (Show, Eq, Ord)

-- Take a map of matrix indexes to area claimed.
-- In other words, some RCIx representing some (i, j)
-- has value `k` if there are `k` claims that land in this 
-- area.
injectClaims :: Map RCIx Int -> Claim -> Map RCIx Int
injectClaims m c = foldl' increment m (claimIxList c)
  where increment m rix = Map.insertWith (+) rix 1 m


-- Parse a singular claim, which in raw texts refers to the format
-- [#][claimId][spaces+][@][space*][xOffset],[yOffset][:][spaces+][width][x][height]
parseClaim :: Parsec () String Claim
parseClaim = do
  single '#'
  cid <- decimal
  space1
  single '@'
  space1
  xoff <- decimal
  single ','
  yoff <- decimal
  single ':'
  space1
  width <- decimal
  single 'x'
  height <- decimal
  pure $ Claim cid xoff yoff width height

-- Turn our claim into a list of 
-- indices for the entire area covered by the particular claim
-- by combining two lists, one for the span of x, and another
-- for the span of y
claimIxList :: Claim -> [RCIx]
claimIxList c =
  let xOff = cXOff c
      yOff = cYOff c
  in  [ RCIx x y
      | x <- [xOff .. (xOff + (cWidth c - 1))]
      , y <- [yOff .. yOff + (cHeight c - 1)]
      ]

-- Ensure the indices that the claim covers
-- are all equal to 1, indicating no overlap. If not,
-- there exists overlap
noOverlap :: Map RCIx Int -> Claim -> Bool
noOverlap m c = and $ (noOverlap' m <$> claimIxList c)
  where noOverlap' m rix = maybe False (== 1) (Map.lookup rix m)

runExercise3 :: FilePath -> IO ()
runExercise3 fp = do
  l <- lines <$> readFile fp
  let x              = traverse (parseMaybe parseClaim) l
      claims         = maybe (error "error parsing") id x
      cAreaMap       = foldl' injectClaims (Map.empty) claims
      overlapping    = length (filter (>= 2) (Map.elems cAreaMap))
      notOverlapping = head $ filter (noOverlap cAreaMap) claims
  putStrLn $ "The # of overlapping blah is " <> show overlapping
  putStrLn $ "The id of the non overlapping one is " <> show
    (cId notOverlapping)
