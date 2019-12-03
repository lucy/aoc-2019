{-# LANGUAGE TupleSections #-}
import Data.List
import qualified Data.Set as S
import qualified Data.Map.Strict as M
--l :: String -> [String]
r ',' = ' '
r c   = c
z :: Int -> Int -> Int -> Char -> ((Int, Int), [(Int, Int)])
z x y n 'U' = ((x, y - n), map (x, ) [y, y - 1 .. y - n])
z x y n 'D' = ((x, y + n), map (x, ) [y .. y + n])
z x y n 'L' = ((x - n, y), map (, y) [x, x - 1 .. x - n])
z x y n 'R' = ((x + n, y), map (, y) [x .. x + n])
f
  :: (Int, Int, Int, S.Set (Int, Int), M.Map (Int, Int) Int)
  -> (Char, Int)
  -> (Int, Int, Int, S.Set (Int, Int), M.Map (Int, Int) Int)
f (x, y, o, l, map) (c, n) | ((x, y), m) <- z x y n c =
  ( x
  , y
  , o + n
  , S.union l (S.fromList m)
  , M.union (M.fromList $ zip m [o ..]) map
  )
d (x, y) = abs x + abs y
g l | (x, y, o, s, m) <- foldl f (0, 0, 0, S.fromList [], M.fromList []) l =
  (s, m)
u :: String -> (Char, Int)
u (c : n) = (c, read n)
l = g . map u . words . map r
k
  :: (S.Set (Int, Int), M.Map (Int, Int) Int)
  -> (S.Set (Int, Int), M.Map (Int, Int) Int)
  -> (Int, Int)
k (as, am) (bs, bm) = (p1, p2)
 where
  p1 = minimum $ filter (/= 0) $ map d $ S.toList $ S.intersection as bs
  p2 = minimum $ filter (/= 0) $ map e $ S.toList $ S.intersection as bs
  m  = M.unionWith (+) am bm
  e (x, y) = (M.!) m (x, y)
s s | (a, _ : b) <- break (== '\n') s = k (l a) (l $ init b)
main = interact $ (++ "\n") . show . s
