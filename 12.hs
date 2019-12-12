{-# LANGUAGE TupleSections #-}
import Text.ParserCombinators.ReadP
import qualified Data.Set as S
import Data.List

type U = [[(Int, Int)]]

parse :: String -> [(Int, Int, Int)]
parse s = fst $ last $ readP_to_S (many p) s
 where
  v c = char c *> char '=' *> readS_to_P reads <* optional (string ", ")
  p = (,,) <$ skipSpaces <* char '<' <*> v 'x' <*> v 'y' <*> v 'z' <* char '>'

sim :: [(Int, Int)] -> [(Int, Int)]
sim l = map (move . foldl1 velocity) $ rotations
  where rotations = take (length l) $ iterate (\l -> (tail l) ++ [head l]) l

velocity :: (Int, Int) -> (Int, Int) -> (Int, Int)
velocity (a, v) (b, _)
  | a < b     = (a, v + 1)
  | a > b     = (a, v - 1)
  | otherwise = (a, v)

move :: (Int, Int) -> (Int, Int)
move (c, v) = (c + v, v)

energy :: U -> Int
energy l = sum (map f (transpose l))
  where f l = let (c, v) = unzip l in sum (map abs c) * sum (map abs v)

findCycles :: [U] -> [Int]
findCycles (  []      : _) = []
findCycles l@((_ : _) : _) = f (map head l) S.empty : findCycles (map tail l)
  where f (x : xs) s = if S.member x s then S.size s else f xs (S.insert x s)

main :: IO ()
main = do
  (x, y, z) <- unzip3 . parse <$> getContents
  let steps = iterate (map sim) (map (map (, 0)) [x, y, z])
  print $ energy (steps !! 1000)
  print $ foldl1 lcm (findCycles steps)
