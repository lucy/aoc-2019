import qualified Data.Map.Strict as M
data P = P Int Int deriving (Eq, Ord)
z (P x y) n 'U' = map (x `P`) [y, y - 1 .. y - n]
z (P x y) n 'D' = map (x `P`) [y .. y + n]
z (P x y) n 'L' = map (`P` y) [x, x - 1 .. x - n]
z (P x y) n 'R' = map (`P` y) [x .. x + n]
f (p, o, s) (c, n) | t <- z p n c = (last t, o + n, zip t [o ..] ++ s)
g l | (_, _, m) <- foldl f (P 0 0, 0, []) l = M.delete (P 0 0) $ M.fromList m
d (P x y) = abs x + abs y
k m = (f M.keys, f M.elems) where f g = minimum $ g $ M.mapKeys d $ m
r ',' = ' '
r c   = c
u (c : n) = (c, read n)
p = g . map u . words . map r
s s | (a, b) <- break (== '\n') s = k $ M.intersectionWith (+) (p a) (p b)
main = interact $ show . s
