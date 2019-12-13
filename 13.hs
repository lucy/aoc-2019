import Data.Semigroup
import qualified Data.Map.Strict as M

type N = Int
type Mem = M.Map N N
data Req = Get (N -> Req) | Put N Req | Done

vm :: Mem -> N -> N -> Req
vm m rb pc = case is `mod` 100 of
  1  -> vm (w p3 $ r p1 + r p2) rb (pc + 4)
  2  -> vm (w p3 $ r p1 * r p2) rb (pc + 4)
  3  -> Get (\v -> vm (w p1 v) rb (pc + 2))
  4  -> Put (r p1) (vm m rb (pc + 2))
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

type Canvas = M.Map (N, N) N

render :: Canvas -> [String]
render m = map line [y .. h]
 where
  line y = map (px y) [x .. w]
  symbols = [' ', '█', '▭', '─', '●']
  px y x = symbols !! M.findWithDefault 0 (x, y) m
  (Min x, Max w, Min y, Max h) = foldl1 (<>) (map f (M.keys m))
  f (x, y) = (Min x, Max x, Min y, Max y)

run' :: Req -> Canvas -> N -> N -> N -> (Canvas, N)
run' r c n b p = case r of
  Done  -> (c, n)
  Get f -> run' (f (signum (b - p))) c n b p
  Put x (Put y (Put t r')) ->
    let
      (c', n') = case (x, y) of
        (-1, 0) -> (c, t)
        _       -> (M.insert (x, y) t c, n)
      (b', p') = case (x, y, t) of
        (-1, 0, _) -> (b, p)
        (x , y, 3) -> (b, x)
        (x , y, 4) -> (x, p)
        _          -> (b, p)
    in run' r' c' n' b' p'

main :: IO ()
main = do
  m <- M.fromList . zip [0 ..] . read . ('[' :) . (++ "]") <$> getContents
  let (p1c, p1s) = run' (vm m 0 0) M.empty 0 0 0
  let (p2c, p2s) = run' (vm (M.insert 0 2 m) 0 0) M.empty 0 0 0
  putStr (unlines $ zipWith (\x y -> x ++ " " ++ y) (render p1c) (render p2c))
  print (length (filter (== 2) $ M.elems p2c))
  print p2s
