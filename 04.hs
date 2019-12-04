import Data.List
f t s = any t (map length (group s)) && length s == 6 && sort s == s
g r = (h (>= 2), h (== 2)) where h t = length . filter (f t . show) r
p s | (x, (_ : y)) <- break (== '-') s = [read x :: Int .. read y]
main = interact $ show . g . p
