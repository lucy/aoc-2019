import Data.Semigroup
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type N = Int
type Mem = IM.IntMap N
data Req = Get (N -> Req) | N :< Req | Done
infixr 5 :<

vm :: Mem -> N -> N -> Req
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
  r i = IM.findWithDefault 0 i m
  w i x = IM.insert i x m
  is = r pc
  ix i p = case is `quot` p `mod` 10 of
    1 -> pc + i
    0 -> r (pc + i)
    2 -> r (pc + i) + rb
  (p1, p2, p3) = (ix 1 100, ix 2 1000, ix 3 10000)

data Tile = Empty | Wall | Oxygen | Unexplored deriving Eq

type Canvas = M.Map (N, N) Tile

render :: S.Set (N, N) -> Canvas -> String
render s m = unlines [ [ px x y | x <- [x .. w] ] | y <- [y .. h] ]
 where
  px x y = case M.findWithDefault Unexplored (x, y) m of
    _ | (x, y) == (0, 0)  -> 'D'
    Oxygen                -> '@'
    _ | S.member (x, y) s -> '*'
    Unexplored            -> ' '
    Empty                 -> '.'
    Wall                  -> '#'
  (Min x, Max w, Min y, Max h) =
    mconcat [ (Min x, Max x, Min y, Max y) | (x, y) <- M.keys m ]

move :: N -> (N, N) -> (N, N)
move 1 (x, y) = (x, y - 1)
move 2 (x, y) = (x, y + 1)
move 3 (x, y) = (x - 1, y)
move 4 (x, y) = (x + 1, y)

run' :: [([(N, N)], Req)] -> [[(N, N)]] -> Canvas -> ([[(N, N)]], Canvas)
run' [] paths c = (paths, c)
run' ((path@(pos : _), st :< Get k) : q) paths c =
  let queue dir = (move dir pos : path, k dir)
  in
    case M.lookup pos c of
      Just Empty -> run' q paths c
      _          -> case st of
        0 -> run' q paths (M.insert pos Wall c)
        1 -> run' (map queue [1 .. 4] ++ q) paths (M.insert pos Empty c)
        2 -> run' q (path : paths) (M.insert pos Oxygen c)

fill :: Canvas -> Int -> Int
fill c n
  | full      = n
  | otherwise = fill (M.mapWithKey fill' c) (succ n)
 where
  adj :: (N, N) -> Bool
  adj pos = any
    (\k -> M.findWithDefault Unexplored k c == Oxygen)
    (map (flip move pos) [1 .. 4])
  fill' :: (N, N) -> Tile -> Tile
  fill' pos t = if t == Empty && adj pos then Oxygen else t
  full = not $ any (== Empty) (M.elems c)

main :: IO ()
main = do
  m <- IM.fromList . zip [0 ..] . read . ('[' :) . (++ "]") <$> getContents
  let (paths, maze) = run' [([(0, 0)], 1 :< vm m 0 0)] [] M.empty
  putStr $ render (S.fromList (paths !! 0)) maze
  print $ length (paths !! 0) - 1
  print $ fill maze 0
