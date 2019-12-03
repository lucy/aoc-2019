{-# LANGUAGE TupleSections #-}
import qualified Data.Map.Strict as M
z :: Int -> Int -> Int -> Char -> [(Int, Int)]
z x y n 'U' = map (x, ) [y, y - 1 .. y - n]
z x y n 'D' = map (x, ) [y .. y + n]
z x y n 'L' = map (, y) [x, x - 1 .. x - n]
z x y n 'R' = map (, y) [x .. x + n]
f
  :: ((Int, Int), Int, M.Map (Int, Int) Int)
  -> (Char, Int)
  -> ((Int, Int), Int, M.Map (Int, Int) Int)
f ((x, y), o, s) (c, n) | t <- z x y n c =
  (last $ t, o + n, M.union (M.fromList $ zip t [o ..]) s)
g :: [(Char, Int)] -> M.Map (Int, Int) Int
g l | (_, _, m) <- foldl f ((0, 0), 0, M.fromList []) l = m
d :: (Int, Int) -> Int
d (x, y) = abs x + abs y
k :: (M.Map (Int, Int) Int) -> (M.Map (Int, Int) Int) -> (Int, Int)
k a b = (f (d . fst), f snd)
 where
  f :: (((Int, Int), Int) -> Int) -> Int
  f g = minimum $ filter (/= 0) $ map g $ M.toList $ M.intersectionWith (+) a b
r :: Char -> Char
r ',' = ' '
r c   = c
u :: String -> (Char, Int)
u (c : n) = (c, read n)
l :: String -> (M.Map (Int, Int) Int)
l = g . map u . words . map r
s :: String -> (Int, Int)
s s | (a, _ : b) <- break (== '\n') s = k (l a) (l $ init b)
main = interact $ (++ "\n") . show . s
