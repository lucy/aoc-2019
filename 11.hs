import Data.Int
import Data.Semigroup
import qualified Data.Map.Strict as M

type N = Int
type Mem = M.Map N N
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
  r i = M.findWithDefault 0 i m
  w i x = M.insert i x m
  is = r pc
  ix i p = case is `quot` p `mod` 10 of
    1 -> pc + i
    0 -> r (pc + i)
    2 -> r (pc + i) + rb
  (p1, p2, p3) = (ix 1 100, ix 2 1000, ix 3 10000)

turn :: N -> (N, N) -> (N, N)
turn 0 (x, y) = (y, -x)
turn 1 (x, y) = (-y, x)

move :: (N, N) -> (N, N) -> (N, N)
move (x, y) (u, v) = (x + u, y + v)

type Canvas = M.Map (N, N) N

draw :: Req -> (N, N) -> (N, N) -> Canvas -> Canvas
draw Done          _ _ m = m
draw (Get k      ) p d m = draw (k $ M.findWithDefault 0 p m) p d m
draw (c :< t :< r) p d m = draw r (move d' p) d' (M.insert p c m)
  where d' = turn t d

run :: N -> Mem -> Canvas
run c m = draw (k c) (0, 0) (0, -1) M.empty where (Get k) = vm m 0 0

render :: Canvas -> String
render m = unlines [ [ px x y | x <- [x .. w] ] | y <- [y .. h] ]
 where
  px x y = if M.findWithDefault 0 (x, y) m == 0 then ' ' else '#'
  (Min x, Max w, Min y, Max h) =
    mconcat [ (Min x, Max x, Min y, Max y) | (x, y) <- M.keys m ]

main :: IO ()
main = do
  m <- M.fromList . zip [0 ..] . read . ('[' :) . (++ "]") <$> getContents
  print (M.size (run 0 m))
  putStr (render (run 1 m))
