{-# LANGUAGE BangPatterns #-}
import Data.Semigroup
import Control.Monad
import Data.Ord
import Data.List
import Debug.Trace
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type N = Int
data Req = Get (N -> Req) | N :< Req | Done
infixr 5 :<

vm :: IM.IntMap N -> N -> N -> Req
vm m rb pc = case is `mod` 100 of
  1  -> vm (w p3 $ r p1 + r p2) rb (pc + 4)
  2  -> vm (w p3 $ r p1 * r p2) rb (pc + 4)
  3  -> Get (\v -> vm (w p1 v) rb (pc + 2))
  4  -> r p1 :< vm m rb (pc + 2)
  5  -> vm m rb (if r p1 /= 0 then r p2 else pc + 3)
  6  -> vm m rb (if r p1 == 0 then r p2 else pc + 3)
  7  -> vm (w p3 $ if r p1 < r p2 then 1 else 0) rb (pc + 4)
  8  -> vm (w p3 $ if r p1 == r p2 then 1 else 0) rb (pc + 4)
  9  -> vm m (rb + r p1) (pc + 2)
  99 -> Done
 where
  r i = IM.findWithDefault 0 (fromIntegral i) m
  w i x = IM.insert (fromIntegral i) x m
  is = r pc
  ix i p = case is `quot` p `mod` 10 of
    1 -> pc + i
    0 -> r (pc + i)
    2 -> r (pc + i) + rb
  (p1, p2, p3) = (ix 1 100, ix 2 1000, ix 3 10000)

type Pos = (N, N)
type Set = S.Set Pos

render :: Set -> Set -> String
render m p = unlines [ [ px (x, y) | x <- [x .. w] ] | y <- [y .. h] ]
 where
  px :: Pos -> Char
  px c
    | S.member c p = '*'
    | S.member c m = '#'
    | otherwise    = '.'
  (Min x, Max w, Min y, Max h) =
    mconcat [ (Min x, Max x, Min y, Max y) | (x, y) <- S.toList m ]

data Dir = N | S | W | E deriving (Enum, Show)

allDirs :: [Dir]
allDirs = enumFrom $ toEnum 0

move :: Pos -> Dir -> Pos
move (x, y) N = (x, y - 1)
move (x, y) S = (x, y + 1)
move (x, y) W = (x - 1, y)
move (x, y) E = (x + 1, y)

p1 :: Req -> (Set, (Dir, Pos), [Pos])
p1 r = go S.empty (N, (0, 0)) (0, 0) r
 where
  ints :: Set -> [Pos]
  ints s = [ (x, y) | p@(x, y) <- S.toList s, t p ]
    where t p = all (\p -> S.member p s) (move p <$> allDirs)
  go :: Set -> (Dir, Pos) -> Pos -> Req -> (Set, (Dir, Pos), [Pos])
  go m b (x, y) (c :< r) = case c' of
    '\n' -> go m b (0, succ y) r
    '#'  -> go (S.insert (x, y) m) b (succ x, y) r
    '.'  -> go m b (succ x, y) r
    _    -> go m (d, (x, y)) (succ x, y) r
     where
      d = case c' of
        '^' -> N
        'v' -> S
        '<' -> W
        '>' -> E
    where c' = toEnum c
  go m b _ _ = (m, b, ints m)


data Move = L | R | Move N deriving (Show, Eq, Ord)

turn :: Move -> Pos -> Pos
turn L (x, y) = (y, -x)
turn R (x, y) = (-y, x)

getTurn :: Pos -> Pos -> [Move]
getTurn m t = if length l < length r then l else r
 where
  l = try L m
  r = try R m
  try d m = if t == m then [] else d : try d (turn d m)

encode :: Dir -> [Pos] -> [Move]
encode dir (start : path) = go 0 (move (0, 0) dir) start path
 where
  go :: N -> Pos -> Pos -> [Pos] -> [Move]
  go n _ _ [] = if n /= 0 then [Move n] else []
  go n dir pos@(x, y) (pos'@(x', y') : ps)
    | dir == dir' = go (succ n) dir' pos' ps
    | otherwise   = m ++ t ++ go 1 dir' pos' ps
   where
    dir' = (x' - x, y' - y)
    t    = getTurn dir dir'
    m    = if n /= 0 then [Move n] else []

search :: Set -> Set -> [(Pos, Pos, [Pos], Set)] -> [[Pos]]
search _ _ []                  = []
search g i ((c, l, p, t) : ps) = case next of
  [] | S.size t == S.size g -> p : search g i ps
  _                         -> search g i (next ++ ps)
 where
  next =
    [ (c', c, (c : p), S.insert c' t)
    | c' <- move c <$> allDirs
    , c' /= c && c' /= l
    , S.member c' g
    , S.member c' i || not (S.member c' t)
    ]

toAscii :: Move -> String
toAscii R        = "R"
toAscii L        = "L"
toAscii (Move n) = show n

encodeAscii :: [Move] -> String
encodeAscii (x : y : ys) = toAscii x ++ "," ++ encodeAscii (y : ys)
encodeAscii (x     : xs) = toAscii x ++ encodeAscii xs
encodeAscii []           = []

count :: Eq a => [a] -> [a] -> Int
count l [] = 0
count l s  = count' l 0
 where
  count' [] n = n
  count' l n
    | s `isPrefixOf` l = count' (drop (length s) l) (succ n)
    | otherwise        = count' (tail l) n

-- R,4,L,10,L,10,L,8,R,12,R,10,R,4,R,4,L,10,L,10,L,8,R,12,R,10,R,4,R,4,L,10,L,10,L,8,L,8,R,10,R,4,L,8,R,12,R,10,R,4,L,8,L,8,R,10,R,4,R,4,L,10,L,10,L,8,L,8,R,10,R,4

-- A L,8,L,8,R,10,R,4
-- B R,4,L,10,L,10
-- C L,8,R,12,R,10,R,4
-- B,C,B,C,B,A,C,A,B,A

--p2 :: Req -> Set -> (Dir, Pos) -> [Pos] -> N
-- p2 r g (p, b) i = search g i [(b, b, [], S.empty)]

input =
  "B,C,B,C,B,A,C,A,B,A\n"
    ++ "L,8,L,8,R,10,R,4\n"
    ++ "R,4,L,10,L,10\n"
    ++ "L,8,R,12,R,10,R,4\nn\n"

--p2 :: Req -> String -> N
p2 (Get k ) (x : xs) = p2 (k (fromEnum x)) xs
p2 (n :< r) l        = n : p2 r l
p2 Done     l        = []
--p2 _        l        = traceShow l 0

main :: IO ()
main = do
  m <- IM.fromList . zip [0 ..] . read . ('[' :) . (++ "]") <$> getContents
  let (g, (p, b), i) = p1 (vm m 0 0)
  print $ sum (map (uncurry (*)) i)
  -- let ps = p2 (IM.insert 0 2 m) g (p, b) (S.fromList i)
  -- print $ length ps
  -- let enc = map (encode p . reverse) ps
  -- print $ head $ sortBy (comparing length) (map encodeAscii enc)
  print $ last (p2 (vm (IM.insert 0 2 m) 0 0) input)
  -- print $ encodeAscii $ head enc
  --print $ compress $ head enc
