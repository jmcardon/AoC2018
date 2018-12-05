module Exercise5 where

import           Data.Char                      ( toUpper
                                                , toLower
                                                , isUpper
                                                , isSpace
                                                )
import           Data.List                      ( sortOn )

-- naiive brute force soln
-- does not eliminate all the input at once, requires multiple passes
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

-- Courtesy of big boi HVR 
-- inspired by zippers, function name
-- sponsored by Fpco
eliminateHVR :: String -> String
eliminateHVR = eliminate' []
 where
  eliminate' done []       = reverse done
  eliminate' []   (x : xs) = eliminate' [x] xs
  eliminate' (x : xs) (y : ys) | canElim x y = eliminate' xs ys
                               | otherwise   = eliminate' (y : x : xs) ys
  canElim a b = (a == toLower b || a == toUpper b) && a /= b

runExercise5 :: FilePath -> IO ()
runExercise5 fp = do
  l <- filter (not . isSpace) <$> readFile fp
  let elim1 = eliminateHVR l
      elim2 =
        head $ sortOn length $ (eliminateHVR . flip remove l) <$> ['a' .. 'z']
  putStrLn $ "Eliminated initial expression length: " <> (show . length) elim1
  putStrLn
    $  "Eliminated expression without 1 polymer min length: "
    <> (show . length) elim2
  where remove a = filter ((/=) a . toLower)
