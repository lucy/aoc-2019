{-# LANGUAGE FlexibleContexts #-}
import Control.Monad
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.List
if' :: Bool -> a -> a -> a
if' t x y = if t then x else y
vm :: STArray s Int Int -> [Int] -> Int -> ST s [Int]
vm m input pc = do
  is <- readArray m pc
  let
    op = is `mod` 100
    i1 = i 1 ((is `quot` 100) `mod` 10)
    i2 = i 2 ((is `quot` 1000) `mod` 10)
    i i 1 = readArray m (pc + i)
    i i 0 = readArray m (pc + i) >>= readArray m
    o i x = readArray m (pc + i) >>= \i' -> writeArray m i' x
    op2 f = liftM2 f i1 i2
  case op of
    1  -> op2 (+) >>= o 3 >> vm m input (pc + 4)
    2  -> op2 (*) >>= o 3 >> vm m input (pc + 4)
    3  -> o 1 (head input) >> vm m (tail input) (pc + 2)
    4  -> liftM2 (:) i1 $ vm m input (pc + 2)
    5  -> op2 (\i1 i2 -> if' (i1 /= 0) i2 (pc + 3)) >>= vm m input
    6  -> op2 (\i1 i2 -> if' (i1 == 0) i2 (pc + 3)) >>= vm m input
    7  -> op2 (\i1 i2 -> if' (i1 >= i2) 0 1) >>= o 3 >> vm m input (pc + 4)
    8  -> op2 (\i1 i2 -> if' (i1 == i2) 1 0) >>= o 3 >> vm m input (pc + 4)
    99 -> return []
    _  -> error $ "invalid opcode: " ++ show is ++ " at " ++ show pc
vm' :: Array Int Int -> [Int] -> [Int]
vm' a input = runST $ thaw a >>= \m -> vm m input 0
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
