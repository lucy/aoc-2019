import Data.List

p1 :: [Int] -> [Int]
p1 s = take 8 $ iterate g s !! 100
 where
  b = [0, 1, 0, -1]
  p = [ tail $ cycle [ x | n <- b, x <- replicate i n ] | i <- [1 ..] ]
  f p s = abs (sum (zipWith (*) p s)) `mod` 10
  g s = zipWith f p (replicate (length s) s)

p2 :: [Int] -> [Int]
p2 s = take 8 $ reverse $ iterate f s' !! 100
 where
  i  = foldl (\x y -> x * 10 + y) 0 (take 7 s)
  s' = reverse $ drop i $ take (length s * 10000) (cycle s)
  f  = map ((`mod` 10) . abs) . tail . scanl' (+) 0

main = do
  s <- getContents
  let l = [ fromEnum c - fromEnum '0' | c <- s, c >= '0' && c <= '9' ]
  putStrLn $ p1 l >>= show
  putStrLn $ p2 l >>= show
