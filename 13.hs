import qualified Data.Map.Strict as M

type N = Int
data Req = Get (N -> Req) | N :< Req | Done
infixr 5 :<

vm :: M.Map N N -> N -> N -> Req
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

-- import Data.Semigroup
--
-- type Canvas = M.Map (N, N) N
--
-- render :: Canvas -> [String]
-- render m = [ [ px x y | x <- [x .. w] ] | y <- [y .. h] ]
--  where
--   symbols = [' ', '█', '▭', '▔', '●']
--   px x y = symbols !! M.findWithDefault 0 (x, y) m
--   (Min x, Max w, Min y, Max h) =
--     mconcat [ (Min x, Max x, Min y, Max y) | (x, y) <- M.keys m ]
--
-- run :: Req -> Canvas -> N -> N -> N -> (Canvas, N)
-- run r c s b p = case r of
--   Done                -> (c, s)
--   Get k               -> run (k (signum (b - p))) c s b p
--   -1 :< 0 :< s' :< r' -> run r' c s' b p
--   x  :< y :< t  :< r' ->
--     run r' (M.insert (x, y) t c) s
--       (if t == 4 then x else b) (if t == 3 then x else p)

p1 :: Req -> N -> N
p1 r n = case r of
  Done              -> n
  _ :< _ :< 2 :< r' -> p1 r' (succ n)
  _ :< _ :< _ :< r' -> p1 r' n

p2 :: Req -> N -> N -> N -> N
p2 r s b p = case r of
  Done                -> s
  Get k               -> p2 (k $ signum (b - p)) s b p
  -1 :< 0 :< s' :< r' -> p2 r' s' b p
  p' :< _ :< 3  :< r' -> p2 r' s b p'
  b' :< _ :< 4  :< r' -> p2 r' s b' p
  _  :< _ :< _  :< r' -> p2 r' s b p

main :: IO ()
main = do
  m <- M.fromList . zip [0 ..] . read . ('[' :) . (++ "]") <$> getContents
  -- let (p1c, p1s) = run (vm m 0 0) M.empty 0 0 0
  -- let (p2c, p2s) = run (vm (M.insert 0 2 m) 0 0) M.empty 0 0 0
  -- putStr (unlines $ zipWith (\x y -> x ++ " " ++ y) (render p1c) (render p2c))
  -- print (length (filter (== 2) $ M.elems p2c))
  -- print p2s
  print $ p1 (vm m 0 0) 0
  print $ p2 (vm (M.insert 0 2 m) 0 0) 0 0 0
