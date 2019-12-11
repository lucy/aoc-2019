{-# LANGUAGE TupleSections, LambdaCase #-}
import qualified Data.Map.Strict as M
z (x, y) n = \case
  'U' -> map (x, ) [y, y - 1 .. y - n]
  'D' -> map (x, ) [y .. y + n]
  'L' -> map (, y) [x, x - 1 .. x - n]
  'R' -> map (, y) [x .. x + n]
f (p, o, s) (c, n) | t <- z p n c = (last t, o + n, zip t [o ..] ++ s)
g l | (_, _, m) <- foldl f ((0, 0), 0, []) l = M.delete (0, 0) $ M.fromList m
d (x, y) = abs x + abs y
k m = (f M.keys, f M.elems) where f = minimum . ($ M.mapKeys d m)
p = g . map (\(c : n) -> (c, read n)) . words . map (\case ',' -> ' '; c -> c)
s s | (a, b) <- break (== '\n') s = k $ M.intersectionWith (+) (p a) (p b)
main = interact $ show . s
