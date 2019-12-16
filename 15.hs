import Data.Semigroup
import qualified Data.IntMap.Strict as IM
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

type Map = S.Set (N, N)

render :: Map -> String
render m = unlines [ [ px (x, y) | x <- [x .. w] ] | y <- [y .. h] ]
 where
  px p = if S.member p m then '.' else ' '
  (Min x, Max w, Min y, Max h) =
    mconcat [ (Min x, Max x, Min y, Max y) | (x, y) <- S.toList m ]

move :: (N, N) -> N -> (N, N)
move (x, y) 1 = (x, y - 1)
move (x, y) 2 = (x, y + 1)
move (x, y) 3 = (x - 1, y)
move (x, y) 4 = (x + 1, y)

run :: [((N, N), N, Req)] -> Map -> (N, N) -> N -> (Map, (N, N), N)
run []                           m o n = (m, o, n)
run ((pos, n', st :< Get k) : q) m o n = case st of
  p | p == 0 || S.member pos m -> run q m o n
  1 -> run (map queue [1 .. 4] ++ q) (S.insert pos m) o n
    where queue dir = (move pos dir, succ n', k dir)
  2 -> run q m pos n'

fill :: Map -> Map -> Int -> Int
fill m o n
  | S.null m  = n
  | otherwise = fill (S.difference m a) (S.union o a) (succ n)
  where a = S.filter (\p -> any (flip S.member o . move p) [1 .. 4]) m

main :: IO ()
main = do
  m <- IM.fromList . zip [0 ..] . read . ('[' :) . (++ "]") <$> getContents
  let (maze, o, n) = run [((0, 0), 0, 1 :< vm m 0 0)] S.empty (0, 0) 0
  putStr $ render maze
  print $ n
  print $ fill maze (S.singleton o) 0
