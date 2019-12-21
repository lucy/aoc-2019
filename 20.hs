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

allDirs :: [Dir]
allDirs = enumFrom $ toEnum 0

move :: Pos -> Dir -> Pos
move (x, y) N = (x, y - 1)
move (x, y) S = (x, y + 1)
move (x, y) W = (x - 1, y)
move (x, y) E = (x + 1, y)

data Tile = Portal String Pos | Open | Wall deriving Show

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
      | (p', Just c') <- (\p' -> (p', M.lookup p' cm)) . move p <$> allDirs
      , isLetter c'
      ]
    [pos] =
      [ pos
      | l   <- [p, p']
      , pos <- move l <$> allDirs
      , M.lookup pos cm == Just '.'
      ]

portal :: Map Pos Tile -> String -> [Pos]
portal m s = nub [ pos | (p, Portal name pos) <- M.toList m, name == s ]

p1 :: Map Pos Tile -> Int
p1 m = minimum $ map fst $ go [(-1, [fp])]
 where
  ([fp], [tp]) = (portal m "AA", portal m "ZZ")
  go :: [(Int, [Pos])] -> [(Int, [Pos])]
  go []                     = []
  go ((n, p@(c : cs)) : ps) = case M.findWithDefault Wall c m of
    _ | elem c cs -> go ps
    Open          -> go (next ++ ps)
      where next = [ (n + 1, c' : p) | c' <- move c <$> allDirs ]
    Portal "AA" pos -> go ps
    Portal "ZZ" pos -> (n, cs) : go ps
    Portal name pos -> go ((n, dest : p) : ps)
      where [dest] = [ p | p <- portal m name, p /= pos ]
    Wall -> go ps

type Pos3 = (Int, Pos)

p2 :: Map Pos Tile -> Int
p2 m = go (S.singleton ((0, fp), -1, S.empty))
 where
  ([fp], [tp]) = (portal m "AA", portal m "ZZ")
  (Min bx, Max bw, Min by, Max bh) =
    mconcat [ (Min x, Max x, Min y, Max y) | ((x, y), Open) <- M.toList m ]
  outer :: Pos -> Bool
  outer (x, y) = x == bx || x == bw || y == by || y == bh
  go :: Set (Pos3, Int, Set Pos3) -> Int
  go (S.deleteFindMin -> ((p@(level, pos), n, v), s))
    | S.member p v = go s
    | level < 0 = go s
    | otherwise = case M.findWithDefault Wall pos m of
      Open -> go (S.union next s)
       where
        next = S.fromList
          [ ((level, pos'), n + 1, S.insert p v)
          | pos' <- move pos <$> allDirs
          ]
      Portal name pos -> case name of
        "AA"              -> go s
        "ZZ" | level == 0 -> n
        "ZZ"              -> go s
        name              -> go (S.insert ((level', dest), n, S.insert p v) s)
         where
          level' = if outer pos then level - 1 else level + 1
          [dest] = [ p | p <- portal m name, p /= pos ]
      Wall -> go s

main = do
  m <- parse <$> getContents
  print $ p1 m
  print $ p2 m
