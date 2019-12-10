import Data.List
import Data.Ord
type Point = (Int, Int)
inLine :: Point -> Point -> Point -> Bool
inLine (ax, ay) (bx, by) (cx, cy) =
  -- check if cross product is 0
  (cy - ay) * (bx - ax) - (cx - ax) * (by - ay) == 0
    && min ax bx <= cx && cx <= max ax bx
    && min ay by <= cy && cy <= max ay by
isVisible :: [Point] -> Point -> Point -> Bool
isVisible l p1 p3 = not $ any (\p2 -> p2 /= p3 && inLine p1 p2 p3) l
visibleFrom :: [Point] -> Point -> [Point]
visibleFrom l p1 = [p3 | p3 <- l, p1 /= p3, isVisible l p1 p3]
angle :: Point -> Point -> Double
angle (px, py) (x, y) = atan2 (-fromIntegral x') (fromIntegral y')
  where (x', y') = (x - px, y - py)
targets :: [Point] -> Point -> [Point]
targets l p = case visibleFrom l p of
  [] -> []
  ts -> ts ++ targets (l \\ ts) p
main :: IO ()
main = do
  s <- getContents
  let l = [(x, y) | (y, l) <- zip [0..] (lines s), (x, '#') <- zip [0..] l]
  let p = maximumBy (comparing (length . visibleFrom l)) l
  print $ length (visibleFrom l p)
  let (x, y) = targets (sortOn (angle p) l) p !! 199
  print $ x * 100 + y
