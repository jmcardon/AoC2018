{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFoldable #-}

module Exercise8
  ( runExercise8
  )
where

import           Text.Megaparsec                ( parseMaybe
                                                , Parsec
                                                )
import           Text.Megaparsec.Char.Lexer     ( decimal )
import           Text.Megaparsec.Char           ( space1
                                                , space
                                                )
import           Control.Monad                  ( replicateM )
import           Data.Foldable                  ( foldl' )
import           Data.Vector                    ( Vector
                                                , (!?)
                                                )
import qualified Data.Vector                   as V

data MultiTree a = Node (Vector (MultiTree a)) [a] | Leaf [a] deriving (Eq, Show, Foldable)

runExercise8 :: FilePath -> IO ()
runExercise8 fp = do
  inp <- filter (/= '\n') <$> readFile fp
  let t      = maybe (error "parseError") id (parseMaybe parseMultiTree inp)
      sumVar = sumMdata t
  putStrLn $ "the part 1 sum is: " <> show sumVar
  putStrLn $ "the part 2 sum is: " <> show (sumOnMetadata t)

sumOnMetadata :: MultiTree Int -> Int
sumOnMetadata (Leaf a      ) = sum a
sumOnMetadata (Node c mdata) = foldl' (smush c) 0 mdata
  where smush c acc i = acc + (maybe 0 sumOnMetadata (c !? (i - 1)))

parseMultiTree :: Parsec () String (MultiTree Int)
parseMultiTree = do
  nChildren <- decimal
  space1
  ndata <- decimal
  space
  f         <- handleParse nChildren
  mdatalist <- parseNData ndata
  pure (f mdatalist)
 where
  parseNData n | n <= 0    = pure []
               | otherwise = replicateM n (decimal <* space)
  handleParse n
    | n == 0    = pure Leaf
    | otherwise = Node . V.fromList <$> replicateM n parseMultiTree

sumMdata :: Num a => MultiTree a -> a
sumMdata = sum
-- sumMdata (Leaf a       ) = sum a
-- sumMdata (Node mtree li) = foldl' (\s t -> s + sumMdata t) (sum li) mtree
