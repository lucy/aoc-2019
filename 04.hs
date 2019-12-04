import Data.List
r = [357253 .. 892942]
f t s = any t (map length (group s)) && sort s == s
h t = length $ filter (f t . show) r
main = print (h (>= 2), h (== 2))
