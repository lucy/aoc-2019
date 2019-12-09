{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.ST
import Data.Array
import Data.Array.ST
import Data.List
import qualified Data.Map.Strict as M
import Data.Int
type Mem = M.Map Int64 Int64
vm :: Mem -> [Int64] -> [Int64]
vm m ch =
  let
    go m ch rb pc =
      let
        r i = M.findWithDefault 0 i m
        w i x = M.insert i x m
        ix i 1 = pc + i
        ix i 0 = r (pc + i)
        ix i 2 = r (pc + i) + rb
        is = r pc
        p1 = ix 1 (is `div` 100 `mod` 10)
        p2 = ix 2 (is `div` 1000 `mod` 10)
        p3 = ix 3 (is `div` 10000 `mod` 10)
      in case is `mod` 100 of
        1 -> go (w p3 $ r p1 + r p2) ch rb (pc + 4)
        2 -> go (w p3 $ r p1 * r p2) ch rb (pc + 4)
        3 -> case ch of (x : xs) -> go (w p1 x) xs rb (pc + 2)
        4 -> r p1 : go m ch rb (pc + 2)
        5 -> go m ch rb (if r p1 /= 0 then r p2 else pc + 3)
        6 -> go m ch rb (if r p1 == 0 then r p2 else pc + 3)
        7 -> go (w p3 $ if r p1 < r p2 then 1 else 0) ch rb (pc + 4)
        8 -> go (w p3 $ if r p1 == r p2 then 1 else 0) ch rb (pc + 4)
        9 -> go m ch (rb + r p1) (pc + 2)
        99 -> []
        _ -> error ("invalid ins: " ++ show is ++ " at " ++ show pc)
  in go m ch 0 0
parse :: String -> Mem
parse s = let l = read ("[" ++ s ++ "]") in M.fromList (zip [0 ..] l)
main :: IO ()
main = do
  m <- parse <$> getContents
  print (last $ vm m [1], last $ vm m [2])
