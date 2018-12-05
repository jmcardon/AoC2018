module Exercise5 where

import           Data.Char                      ( toUpper
                                                , toLower
                                                )
import qualified Data.Text                     as T
import           Data.List                      ( sortOn )
-- import           Data.Function                  ( fix )



eliminate :: String -> String
eliminate = fixedPt eliminate'
 where
  fixedPt f x = let x' = f x in if x == x' then x else fixedPt f x'
  eliminate' (x : y : z : xs) | doesCancel x y = eliminate' (z : xs)
                              | doesCancel y z = eliminate' (x : xs)
                              | otherwise      = x : eliminate' (y : z : xs)
  eliminate' (x : y : xs) | doesCancel x y = eliminate' xs
                          | otherwise      = x : eliminate' (y : xs)
  eliminate' s = s
  doesCancel a b = (a == toLower b || a == toUpper b) && a /= b

runExercise5 :: FilePath -> IO ()
runExercise5 fp = do
  l <- T.unpack . T.strip . T.pack <$> readFile fp
  let elim1 = eliminate l
      elim2 =
        head $ sortOn length $ (eliminate . flip remove l) <$> ['a' .. 'z']
  putStrLn $ "Eliminated initial expression length: " <> (show . length) elim1
  putStrLn
    $  "Eliminated expression without 1 polymer min length: "
    <> (show . length) elim2
  where remove a = filter ((/=) a . toLower)
