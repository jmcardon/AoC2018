{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}


module Exercise9 where

import qualified Data.Map.Strict               as Map
import           Data.List                      ( sortOn )
import           Data.IORef
import           Control.Monad                  ( foldM )
import           Control.Lens                   ( makeLenses
                                                , (^.)
                                                , set
                                                )
data Node a = Node {
    _nodeVal :: a,
    _nodeNext :: IORef (Maybe (Node a)),
    _nodePrev :: IORef (Maybe (Node a))
} deriving (Eq)

instance Show a => Show (Node a) where
    show (Node v _ _) = show v

makeLenses ''Node

setNext :: Node a -> Node a -> IO ()
setNext c nxt = writeIORef (c ^. nodeNext) (Just nxt)

setPrev :: Node a -> Node a -> IO ()
setPrev c nxt = writeIORef (c ^. nodePrev) (Just nxt)

readNext :: Node a -> IO (Maybe (Node a))
readNext c = readIORef (c ^. nodeNext)

readPrev :: Node a -> IO (Maybe (Node a))
readPrev c = readIORef (c ^. nodePrev)

readNext' :: Node a -> IO (Node a)
readNext' c = maybe (error "no next") id <$> readIORef (c ^. nodeNext)

readPrev' :: Node a -> IO (Node a)
readPrev' c = maybe (error "no prev") id <$> readIORef (c ^. nodePrev)

data LinkedList a = LinkedList {
    _llHead :: Node a,
    _llCurrent :: Node a
} deriving (Eq, Show)

makeLenses ''LinkedList

mkEmpty :: a -> IO (Node a)
mkEmpty a = do
  n <- newIORef Nothing
  p <- newIORef Nothing
  pure (Node a n p)

mkHead :: a -> IO (Node a)
mkHead a = do
  h <- mkEmpty a
  writeIORef (h ^. nodeNext) (Just h)
  writeIORef (h ^. nodePrev) (Just h)
  pure h

mkEmptyList :: a -> IO (LinkedList a)
mkEmptyList a = do
  h <- mkHead a
  pure (LinkedList h h)

cycleLL :: Int -> LinkedList a -> IO (LinkedList a)
cycleLL i ll = do
  let c = ll ^. llCurrent
  ncurrent <- cycle' i c
  pure $ set llCurrent ncurrent ll
 where
  cycle' i c | i == 0    = pure c
             | i > 0     = maybe (pure c) (cycle' (i - 1)) =<< readNext c
             | otherwise = maybe (pure c) (cycle' (i + 1)) =<< readPrev c

insertRight :: Node a -> LinkedList a -> IO (LinkedList a)
insertRight n ll = do
  let curr = ll ^. llCurrent
  next <- readNext' curr
  setPrev n curr
  setNext n    next
  setNext curr n
  setPrev next n
  pure $ set llCurrent n ll

-- Remove the current element, return the removed element
removeCurrent :: Eq a => LinkedList a -> IO (LinkedList a, Node a)
removeCurrent ll = do
  let curr = ll ^. llCurrent
      head = ll ^. llHead
  next <- readNext' curr
  prev <- readPrev' curr
  setNext prev next
  setPrev next prev
  let newHead = if (curr == head) then next else head
  pure (LinkedList newHead next, curr)

lastMarble :: Int
lastMarble = 72059

lastMarblePt2 :: Int
lastMarblePt2 = lastMarble * 100

nplayers :: Int
nplayers = 411

runExercise9 :: FilePath -> IO ()
runExercise9 _ = do
  ll1 <- mkEmptyList 0
  ll2 <- mkEmptyList 0
  p1v <- maxValue <$> foldM (combine) (ll1, Map.empty) [1 .. lastMarble]
  p2v <- maxValue <$> foldM (combine) (ll2, Map.empty) [1 .. lastMarblePt2]
  putStrLn $ "The value for pt1 is: " <> show p1v
  putStrLn $ "The value for pt2 is: " <> show p2v
 where
  combine (ll, m) i | i `mod` 23 == 0 = handleSpecial i ll m
                    | otherwise       = handleNormal i ll m
  handleSpecial i ll m = do
    ll0        <- cycleLL (-7) ll
    (ll1, old) <- removeCurrent ll0
    let updated = Map.insertWith (+) (i `mod` nplayers) (i + old ^. nodeVal) m
    pure (ll1, updated)
  handleNormal i ll m = do
    ll0 <- cycleLL 1 ll
    n   <- mkEmpty i
    ll1 <- insertRight n ll0
    pure (ll1, m)
  maxValue (_, m) = head $ sortOn negate (Map.elems m)
