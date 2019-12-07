{-# LANGUAGE FlexibleContexts, BlockArguments #-}
import Control.Monad.ST
import Data.Array.ST
import Data.Array
import Data.List
vm :: STArray s Int Int -> [Int] -> Int -> ST s [Int]
vm m input pc = do
  is <- readArray m pc
  let
    op = is `mod` 100
    m1 = ((is `quot` 100) `mod` 10)
    m2 = ((is `quot` 1000) `mod` 10)
    i i 1 = readArray m (pc + i)
    i i 0 = readArray m (pc + i) >>= readArray m
    o i x = readArray m (pc + i) >>= \i' -> writeArray m i' x
  case op of
    1 -> do
      i1 <- i 1 m1
      i2 <- i 2 m2
      o 3 (i1 + i2)
      vm m input (pc + 4)
    2 -> do
      i1 <- i 1 m1
      i2 <- i 2 m2
      o 3 (i1 * i2)
      vm m input (pc + 4)
    3 -> do
      o 1 (head input)
      vm m (tail input) (pc + 2)
    4 -> do
      i1     <- i 1 m1
      output <- vm m input (pc + 2)
      return (i1 : output)
    5 -> do
      i1 <- i 1 m1
      i2 <- i 2 m2
      vm m input (if i1 /= 0 then i2 else pc + 3)
    6 -> do
      i1 <- i 1 m1
      i2 <- i 2 m2
      vm m input (if i1 == 0 then i2 else pc + 3)
    7 -> do
      i1 <- i 1 m1
      i2 <- i 2 m2
      o 3 (if i1 < i2 then 1 else 0)
      vm m input (pc + 4)
    8 -> do
      i1 <- i 1 m1
      i2 <- i 2 m2
      o 3 (if i1 == i2 then 1 else 0)
      vm m input (pc + 4)
    99 -> return []
    _  -> error $ "invalid opcode: " ++ show is ++ " " ++ show pc
vm' :: Array Int Int -> [Int] -> [Int]
vm' a input = runST do
  m <- thaw a :: ST s (STArray s Int Int)
  vm m input 0
amp :: Array Int Int -> [Int] -> Int
amp m (x : xs) = last e
 where
  f p s = vm' m (p : s)
  e = foldr f a xs
  a = vm' m (x : 0 : e)
f :: Array Int Int -> [Int] -> Int
f m p = maximum $ map (amp m) (permutations p)
parse :: String -> Array Int Int
parse s = let l = read $ "[" ++ s ++ "]" in listArray (0, length l) l
main = do
  m <- parse <$> getContents
  print $ f m [0 .. 4]
  print $ f m [5 .. 9]
