import Data.List
import Text.ParserCombinators.ReadP
import qualified Data.Set as S

parse :: String -> [[Int]]
parse s = fst (last (readP_to_S (many p) s))
 where
  v c = char c *> char '=' *> readS_to_P reads <* optional (string ", ")
  p = skipSpaces *> char '<' *> mapM v ['x', 'y', 'z'] <* char '>'

sim :: [(Int, Int)] -> [(Int, Int)]
sim l = map (move . foldl1 velocity) rotations
  where rotations = take (length l) (iterate (\l -> tail l ++ [head l]) l)

velocity :: (Int, Int) -> (Int, Int) -> (Int, Int)
velocity (a, v) (b, _)
  | a < b     = (a, v + 1)
  | a > b     = (a, v - 1)
  | otherwise = (a, v)

move :: (Int, Int) -> (Int, Int)
move (c, v) = (c + v, v)

energy :: [(Int, Int)] -> Int
energy l = sum (map abs c) * sum (map abs v) where (c, v) = unzip l

findCycles :: [[[(Int, Int)]]] -> [Int]
findCycles (  []      : _) = []
findCycles l@((_ : _) : _) = f (map head l) S.empty : findCycles (map tail l)
  where f (x : xs) s = if S.member x s then S.size s else f xs (S.insert x s)

main :: IO ()
main = do
  coords <- map (map (\x -> (x, 0))) . transpose . parse <$> getContents
  let steps = iterate (map sim) coords
  print $ sum (map energy (transpose (steps !! 1000)))
  print $ foldl1 lcm (findCycles steps)
