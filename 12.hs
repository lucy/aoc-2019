import Data.List
import Text.ParserCombinators.ReadP
import qualified Data.Set as S

parse :: String -> [[Int]]
parse s = fst (last (readP_to_S (many p) s))
 where
  v c = char c *> char '=' *> readS_to_P reads <* optional (string ", ")
  p = skipSpaces *> char '<' *> mapM v ['x', 'y', 'z'] <* char '>'

sim :: [(Int, Int)] -> [(Int, Int)]
sim l =
  [ (a + v', v')
  | (a, v) <- l
  , let v' = v + sum [ signum (b - a) | (b, _) <- l ]
  ]

energy :: [(Int, Int)] -> Int
energy l = sum (map abs c) * sum (map abs v) where (c, v) = unzip l

findCycle :: (Ord a) => S.Set a -> [a] -> Int
findCycle s (x : xs)
  | S.member x s = S.size s
  | otherwise    = findCycle (S.insert x s) xs

main :: IO ()
main = do
  u <- map (map (\x -> (x, 0))) . transpose . parse <$> getContents
  print $ sum (map energy (transpose (iterate (map sim) u !! 1000)))
  print $ foldl1 lcm (map (findCycle S.empty) (map (iterate sim) u))
