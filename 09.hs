import Data.Int
import qualified Data.Map.Strict as M
vm :: M.Map Int64 Int64 -> [Int64] -> Int64 -> Int64 -> [Int64]
vm m ch rb pc = case is `mod` 100 of
  1 -> vm (w p3 $ r p1 + r p2) ch rb (pc + 4)
  2 -> vm (w p3 $ r p1 * r p2) ch rb (pc + 4)
  3 -> vm (w p1 (head ch)) (tail ch) rb (pc + 2)
  4 -> r p1 : vm m ch rb (pc + 2)
  5 -> vm m ch rb (if r p1 /= 0 then r p2 else pc + 3)
  6 -> vm m ch rb (if r p1 == 0 then r p2 else pc + 3)
  7 -> vm (w p3 $ if r p1 < r p2 then 1 else 0) ch rb (pc + 4)
  8 -> vm (w p3 $ if r p1 == r p2 then 1 else 0) ch rb (pc + 4)
  9 -> vm m ch (rb + r p1) (pc + 2)
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
main = do
  m <- M.fromList . zip [0..] . read . ('[':) . (++"]") <$> getContents
  print (last $ vm m [1] 0 0, last $ vm m [2] 0 0)
