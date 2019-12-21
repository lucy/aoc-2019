{-# LANGUAGE ViewPatterns #-}
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Pos = (Int, Int)

data Dir = N | S | W | E deriving (Enum, Show)

move :: Pos -> Dir -> Pos
move (x, y) N = (x, y - 1)
move (x, y) S = (x, y + 1)
move (x, y) W = (x - 1, y)
move (x, y) E = (x + 1, y)

adj :: Pos -> [Pos]
adj p = move p <$> [N, S, W, E]

data Tile = Portal String Pos | Open deriving Show

parse :: String -> Map Pos Tile
parse s = tm
 where
  cm = M.fromList
    [ ((x, y), c) | (l, y) <- zip (lines s) [0 ..], (c, x) <- zip l [0 ..] ]
  tm = M.fromList [ e | (tile -> Just e) <- M.toList cm ]
  tile :: (Pos, Char) -> Maybe (Pos, Tile)
  tile (p, c) = case c of
    '.'            -> Just (p, Open)
    c | isLetter c -> Just (p, portal p c)
    _              -> Nothing
  portal :: Pos -> Char -> Tile
  portal p c = Portal name pos
   where
    name = map snd $ sort [(p, c), (p', c')]
    [(p', c')] =
      [ (p', c')
      | (p', Just c') <- (\p' -> (p', M.lookup p' cm)) <$> adj p
      , isLetter c'
      ]
    [pos] = [ pos | l <- [p, p'], pos <- adj l, M.lookup pos cm == Just '.' ]

portal :: Map Pos Tile -> String -> [Pos]
portal m s = nub [ pos | (p, Portal name pos) <- M.toList m, name == s ]

p1 :: Map Pos Tile -> Int
p1 m = minimum $ go [(fp, -1, S.empty)]
 where
  ([fp], [tp]) = (portal m "AA", portal m "ZZ")
  go :: [(Pos, Int, S.Set Pos)] -> [Int]
  go [] = []
  go ((p, n, v) : s)
    | S.member p v = go s
    | Just t <- M.lookup p m = case t of
      Open -> go (next ++ s)
        where next = [ (p', n + 1, S.insert p v) | p' <- adj p ]
      Portal "AA" _   -> go s
      Portal "ZZ" _   -> n : go s
      Portal name pos -> go ((dest, n, S.insert p v) : s)
        where [dest] = [ p | p <- portal m name, p /= pos ]
    | otherwise = go s

p2 :: Map Pos Tile -> Int
p2 m = go (S.singleton ((0, fp), -1, S.empty))
 where
  ([fp], [tp]) = (portal m "AA", portal m "ZZ")
  (Min bx, Max bw, Min by, Max bh) =
    mconcat [ (Min x, Max x, Min y, Max y) | ((x, y), Open) <- M.toList m ]
  outer :: Pos -> Bool
  outer (x, y) = x == bx || x == bw || y == by || y == bh
  go :: Set ((Int, Pos), Int, Set (Int, Pos)) -> Int
  go (S.deleteFindMin -> ((q@(l, p), n, v), s))
    | l < 0 = go s
    | S.member q v = go s
    | Just t <- M.lookup p m = case t of
      Open -> go (S.union (S.fromList next) s)
        where next = [ ((l, p'), n + 1, S.insert q v) | p' <- adj p ]
      Portal "AA" p -> go s
      Portal "ZZ" p -> if l == 0 then n else go s
      Portal name p -> go (S.insert ((l', p'), n, S.insert q v) s)
       where
        l'   = if outer p then l - 1 else l + 1
        [p'] = [ p' | p' <- portal m name, p' /= p ]
    | otherwise = go s

main = do
  m <- parse <$> getContents
  print $ p1 m
  print $ p2 m
