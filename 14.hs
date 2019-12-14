{-# LANGUAGE TupleSections #-}
import Data.Char
import Data.Map.Strict (Map)
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as M

type Reaction = ([Component], Component)
type Component = (Int, String)

parse :: String -> [Reaction]
parse s = fst $ last $ readP_to_S (sepBy r (char '\n')) s
 where
  r = (,) <$> sepBy1 c (string ", ") <* string " => " <*> c
  c = (,) <$> readS_to_P reads <* char ' ' <*> many1 (satisfy isAsciiUpper)

run'
  :: Map String (Int, [Component])
  -> Int
  -> [Component]
  -> Map String Int
  -> Int
run' _ ore []                _ = ore
run' m ore ((c, "ORE") : xs) l = run' m (ore + c) xs l
run' m ore ((c, n    ) : xs) l = run' m ore (ins' ++ xs) l'
 where
  (makes, ins) = m M.! n
  need         = c - M.findWithDefault 0 n l
  make         = (need + makes - 1) `quot` makes
  l'           = M.insert n (make * makes - need) l
  ins'         = map (\(c, n) -> (c * make, n)) ins

run :: Map String (Int, [Component]) -> Int -> Int
run m n = run' m 0 [(n, "FUEL")] M.empty

search :: Int -> (Int -> Int) -> Int
search t f = s1 0 1
 where
  s1 n0 n1
    | f n1 < t  = s1 n1 (n1 * 2)
    | otherwise = s2 n0 n1
  s2 n0 n1
    | n0 == n1  = n0 - 1
    | f n < t   = s2 (n + 1) n1
    | otherwise = s2 n0 n
    where n = (n0 + n1) `quot` 2

main = do
  l <- parse <$> getContents
  let m = M.fromList $ map (\(ins, (c, n)) -> (n, (c, ins))) l
  print $ run m 1
  print $ search 1000000000000 (run m)
