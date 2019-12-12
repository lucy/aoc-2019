import Text.ParserCombinators.ReadP
import qualified Data.Set as S

data V3 = V3 { _x, _y, _z :: Int } deriving (Eq, Ord)

parse :: String -> [V3]
parse s = fst $ last $ readP_to_S (many p) s
 where
  v c = char c *> char '=' *> readS_to_P reads <* optional (string ", ")
  p = V3 <$ skipSpaces <* char '<' <*> v 'x' <*> v 'y' <*> v 'z' <* char '>'

sim :: [(V3, V3)] -> [(V3, V3)]
sim l = map (move . foldl1 velocity) $ rotations
  where rotations = take (length l) $ iterate (\l -> (last l) : (init l)) l

velocity :: (V3, V3) -> (V3, V3) -> (V3, V3)
velocity (a@(V3 x y z), V3 xv yv zv) (V3 u v h, _) =
  (a, V3 (vel x u xv) (vel y v yv) (vel z h zv))
 where
  vel a b v
    | a < b     = v + 1
    | a > b     = v - 1
    | otherwise = v

move :: (V3, V3) -> (V3, V3)
move (V3 x y z, V3 xv yv zv) = (V3 (x + xv) (y + yv) (z + zv), V3 xv yv zv)

energy :: (V3, V3) -> Int
energy (V3 x y z, V3 xv yv zv) =
  (abs x + abs y + abs z) * (abs xv + abs yv + abs zv)

findCycle :: Ord a => [a] -> Int
findCycle l = f l S.empty
  where f (x : xs) s = if S.member x s then S.size s else f xs (S.insert x s)

main :: IO ()
main = do
  vs <- parse <$> getContents
  let steps = iterate sim (zip vs (repeat (V3 0 0 0)))
  print $ sum $ map energy (steps !! 1000)
  let c f = findCycle $ map (map (\(c, v) -> (f c, f v))) steps
  print $ foldl1 lcm [c _x, c _y, c _z]
