import qualified Data.Map.Strict as M
import Data.List
p = M.fromList . map ((\(a, (_ : b)) -> (b, a)) . break (== ')')) . lines
f m = (p1, p2)
 where
  p1 = sum $ map (subtract 1 . length . t) $ M.keys m
  p2 = i you + i san
  t x = x : (maybe [] t $ m M.!? x)
  (you, san) = (t "YOU", t "SAN")
  off = length . filter (uncurry (==)) $ zip (reverse you) (reverse san)
  i l | Just i <- elemIndex (l !! succ off) l = length l - i
main = interact $ show . f . p
