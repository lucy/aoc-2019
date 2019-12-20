{-# LANGUAGE ViewPatterns #-}
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

put :: N -> Req -> Req
put x (Get k) = k x

test :: N -> N -> Req -> Bool
test x y (put x -> put y -> p :< r) = p == 1

p1 :: Req -> Int
p1 r = length [ (x, y) | x <- [0 .. 49], y <- [0 .. 49], test x y r ]

--p2 :: Req -> Int
p2 r = let (x, y) = z1 0 0 1 in x * 10000 + y
 where
  t x y = test x y r
  f x y = t (x + 99) y && t x (y + 99)
  z1 x y n
    | n >= 99 && f x y = s x y
    | t x'' y          = z1 x'' y n'
    | t x' y           = z1 x' y n
    | otherwise        = (z2 x' y n)
   where
    n'  = n * 2
    x'  = x + n
    x'' = x + n'
  z2 x y n
    | n >= 99 && f x y = s x y
    | t x y''          = z2 x y'' n'
    | t x y'           = z2 x y' n
    | otherwise        = (z1 x y' n)
   where
    n'  = n * 2
    y'  = y + n
    y'' = y + n'
  s x y
    | t x' y' && f x' y'   = s x' y'
    | t x' y && f x' y     = s x' y
    | t x y' && f x y'     = s x y'
    | t x'' y' && f x'' y' = s x'' y'
    | t x'' y && f x'' y   = s x'' y
    | t x' y'' && f x' y'' = s x' y''
    | t x y'' && f x y''   = s x y''
    | otherwise            = (x, y)
   where
    x'  = pred x
    y'  = pred y
    x'' = pred x'
    y'' = pred y'

render :: Set -> String
render m = unlines [ [ px (x, y) | x <- [x .. w] ] | y <- [y .. h] ]
 where
  px :: Pos -> Char
  px c
    | S.member c m = '#'
    | otherwise    = '.'
  (Min x, Max w, Min y, Max h) =
    mconcat [ (Min x, Max x, Min y, Max y) | (x, y) <- S.toList m ]

main :: IO ()
main = do
  m <- IM.fromList . zip [0 ..] . read . ('[' :) . (++ "]") <$> getContents
  print $ p1 (vm m 0 0)
  print $ p2 (vm m 0 0)

