{-# LANGUAGE TupleSections #-}
import qualified Data.Map.Strict as M
z (x, y) n 'U' = map (x, ) [y, y - 1 .. y - n]
z (x, y) n 'D' = map (x, ) [y .. y + n]
z (x, y) n 'L' = map (, y) [x, x - 1 .. x - n]
z (x, y) n 'R' = map (, y) [x .. x + n]
f (p, o, s) (c, n) | t <- z p n c =
  (last t, o + n, M.union (M.fromList $ zip t [o ..]) s)
g l | (_, _, m) <- foldl f ((0, 0), 0, M.empty) l = m
d (x, y) = abs x + abs y
k a b = (f M.keys, f M.elems)
 where
  f g = minimum $ filter (/= 0) $ g $ M.mapKeys d $ M.intersectionWith (+) a b
r ',' = ' '
r c   = c
u (c : n) = (c, read n)
p = map u . words . map r
s s | (a, _ : b) <- break (== '\n') s = k (g $ p a) (g . p $ init b)
main = interact $ show . s
