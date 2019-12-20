import qualified Data.IntMap.Strict as IM

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

test :: N -> N -> Req -> Bool
test x y (Get k) | (Get k') <- k x, p :< r <- k' y = p == 1

p1 :: Req -> N
p1 r = length [ (x, y) | x <- [0 .. 49], y <- [0 .. 49], test x y r ]

p2 :: Req -> N -> N -> N
p2 r x y
  | test (x + 99) (y - 99) r = x * 10000 + (y - 99)
  | test x (succ y) r        = p2 r x (succ y)
  | otherwise                = p2 r (succ x) y

main :: IO ()
main = do
  m <- IM.fromList . zip [0 ..] . read . ('[' :) . (++ "]") <$> getContents
  print $ p1 (vm m 0 0)
  print $ p2 (vm m 0 0) 0 100
