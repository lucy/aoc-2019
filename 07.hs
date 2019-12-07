{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.List
vm :: Array Int Int -> [Int] -> [Int]
vm a input = runST $ do
  m <- thaw a :: ST s (STArray s Int Int)
  let go input pc = do
      ins <- readArray m pc
      let
        read i 1 = readArray m (pc + i)
        read i 0 = readArray m (pc + i) >>= readArray m
        write i x = readArray m (pc + i) >>= \i' -> writeArray m i' x
        op = ins `mod` 100
        i1 = read 1 ((ins `quot` 100) `mod` 10)
        i2 = read 2 ((ins `quot` 1000) `mod` 10)
      case op of
        1 -> do x <- i1; y <- i2; write 3 (x + y); go input (pc + 4)
        2 -> do x <- i1; y <- i2; write 3 (x * y); go input (pc + 4)
        3 -> do write 1 (head input); go (tail input) (pc + 2)
        4 -> do x <- i1; xs <- go input (pc + 2); return (x : xs)
        5 -> do x <- i1; y <- i2; go input (if x /= 0 then y else pc + 3)
        6 -> do x <- i1; y <- i2; go input (if x == 0 then y else pc + 3)
        7 -> do x <- i1; y <- i2; write 3 (if x < y then 1 else 0); go input (pc + 4)
        8 -> do x <- i1; y <- i2; write 3 (if x == y then 1 else 0); go input (pc + 4)
        99 -> return []
        _  -> error $ "invalid opcode: " ++ show ins ++ " " ++ show pc
  go input 0
amp :: Array Int Int -> [Int] -> Int
amp m (x : xs) = last e
 where
  f p s = vm m (p : s)
  e = foldr f a xs
  a = vm m (x : 0 : e)
f :: Array Int Int -> [Int] -> Int
f m p = maximum $ map (amp m) (permutations p)
parse :: String -> Array Int Int
parse s = let l = read $ "[" ++ s ++ "]" in listArray (0, length l) l
main :: IO ()
main = do
  m <- parse <$> getContents
  print $ f m [0 .. 4]
  print $ f m [5 .. 9]
