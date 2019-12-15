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
  r i = IM.findWithDefault 0 (fromIntegral i) m
  w i x = IM.insert (fromIntegral i) x m
  is = r pc
  ix i p = case is `quot` p `mod` 10 of
    1 -> pc + i
    0 -> r (pc + i)
    2 -> r (pc + i) + rb
  (p1, p2, p3) = (ix 1 100, ix 2 1000, ix 3 10000)

data Tile = Empty | Oxygen | Unexplored | Wall deriving Eq

type Canvas = M.Map (N, N) Tile

render :: (N, N) -> S.Set (N, N) -> Canvas -> String
render b s m = unlines [ [ px (x, y) | x <- [x .. w] ] | y <- [y .. h] ]
 where
  px p = case M.findWithDefault Unexplored p m of
    _ | p == b       -> 'D'
    _ | S.member p s -> '*'
    Empty            -> '.'
    Oxygen           -> '@'
    Unexplored       -> ' '
    Wall             -> '#'
  (Min x, Max w, Min y, Max h) =
    mconcat [ (Min x, Max x, Min y, Max y) | (x, y) <- M.keys m ]

move :: (N, N) -> N -> (N, N)
move (x, y) 1 = (x, y - 1)
move (x, y) 2 = (x, y + 1)
move (x, y) 3 = (x - 1, y)
move (x, y) 4 = (x + 1, y)

run :: [([(N, N)], Req)] -> Canvas -> [(N, N)] -> (Canvas, [(N, N)])
run [] m p = (m, p)
run ((p'@(pos : _), st :< Get k) : q) m p = case st of
  _ | Just Empty <- m M.!? pos -> run q m p
  0 -> run q (M.insert pos Wall m) p
  1 -> run (map queue [1 .. 4] ++ q) (M.insert pos Empty m) p
    where queue dir = (move pos dir : p', k dir)
  2 -> run q (M.insert pos Oxygen m) (tail p')

fill :: Canvas -> Int -> Int
fill m n | not $ any (== Empty) (M.elems m) = n
fill m n = fill (M.mapWithKey fill' m) (succ n)
 where
  adj :: (N, N) -> Bool
  adj pos = any (== Just Oxygen) (map ((m M.!?) . move pos) [1 .. 4])
  fill' :: (N, N) -> Tile -> Tile
  fill' pos t = if t == Empty && adj pos then Oxygen else t

main :: IO ()
main = do
  m <- IM.fromList . zip [0 ..] . read . ('[' :) . (++ "]") <$> getContents
  let (maze, path) = run [([(0, 0)], 1 :< vm m 0 0)] M.empty []
  putStr $ render (0, 0) (S.fromList path) maze
  print $ length path
  print $ fill maze 0
