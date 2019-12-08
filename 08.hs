import Data.Char
import Data.Function
import Data.List
import Data.Ord
(h, w) = (25, 6)
chunksOf s [] = []
chunksOf n s  = (take n s) : (chunksOf n (drop n s))
image = chunksOf (h * w) . filter isDigit
count x = length . filter (== x)
p1 = (\l -> count '1' l * count '2' l) . minimumBy (compare `on` count '0')
combine a b = case a of '2' -> b; _ -> a
pixel c = case c of '0' -> ' '; '1' -> '*'
render = unlines . chunksOf h . map pixel
p2 = render . foldl1 (zipWith combine)
main = do
  i <- image <$> getContents
  print $ p1 i;
  putStr $ p2 i
