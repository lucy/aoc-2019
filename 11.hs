import Data.Int
import Data.Semigroup
import qualified Data.Map.Strict as M

type N = Int64
type Mem = M.Map N N

vm :: Mem -> [N] -> N -> N -> [N]
vm m ch rb pc = case is `mod` 100 of
  1  -> vm (w p3 $ r p1 + r p2) ch rb (pc + 4)
  2  -> vm (w p3 $ r p1 * r p2) ch rb (pc + 4)
  3  -> vm (w p1 (head ch)) (tail ch) rb (pc + 2)
  4  -> r p1 : vm m ch rb (pc + 2)
  5  -> vm m ch rb (if r p1 /= 0 then r p2 else pc + 3)
  6  -> vm m ch rb (if r p1 == 0 then r p2 else pc + 3)
  7  -> vm (w p3 $ if r p1 < r p2 then 1 else 0) ch rb (pc + 4)
  8  -> vm (w p3 $ if r p1 == r p2 then 1 else 0) ch rb (pc + 4)
  9  -> vm m ch (rb + r p1) (pc + 2)
  99 -> []
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

draw :: [N] -> (N, N) -> (N, N) -> Canvas -> [(N, Canvas)]
draw []           _ _ _ = []
draw (c : t : xs) p d m = (v, m') : draw xs p' d' m'
 where
  v  = M.findWithDefault 0 p' m
  m' = M.insert p c m
  d' = turn t d
  p' = move d' p

run :: N -> Mem -> Canvas
run c m = snd (last i)
 where
  o = vm m (c : map fst i) 0 0
  i = draw o (0, 0) (0, -1) M.empty

render :: Canvas -> String
render m = unlines (map line [y .. h])
 where
  line y = map (\x -> px x y) [x .. w]
  px x y = if M.findWithDefault 0 (x, y) m == 0 then ' ' else '#'
  (Min x, Max w, Min y, Max h) = foldl1 (<>) (map f (M.keys m))
  f (x, y) = (Min x, Max x, Min y, Max y)

main :: IO ()
main = do
  m <- M.fromList . zip [0 ..] . read . ('[' :) . (++ "]") <$> getContents
  print (M.size (run 0 m))
  putStr (render (run 1 m))
