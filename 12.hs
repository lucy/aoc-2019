import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

parse :: String -> [[Int]]
parse s = fst (last (readP_to_S (many p) s))
 where
  v c = char c *> char '=' *> readS_to_P reads <* optional (string ", ")
  p = skipSpaces *> char '<' *> mapM v ['x', 'y', 'z'] <* char '>'

sim :: [(Int, Int)] -> [(Int, Int)]
sim l = [ (a + v', v') | (a, v) <- l
        , let v' = v + sum [ signum (b - a) | (b, _) <- l ] ]

energy :: [(Int, Int)] -> Int
energy l = sum (map abs c) * sum (map abs v) where (c, v) = unzip l

main :: IO ()
main = do
  u <- map (map (\x -> (x, 0))) . transpose . parse <$> getContents
  print $ sum (map energy (transpose (iterate (map sim) u !! 1000)))
  print $ foldl1 lcm $ map succ $ catMaybes $
    zipWith elemIndex u (tail . iterate sim <$> u)
