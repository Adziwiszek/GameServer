module Utils where

import Data.Maybe (listToMaybe)
import Control.Monad.Cont
import Control.Monad.Random
import System.IO

remove :: Eq a => a -> [a] -> [a]
remove e xs = rm xs []
  where 
    rm [] _ = xs
    rm (x:xs') acc = if e == x then reverse acc ++ xs' else rm xs' (x:acc) 


member :: Eq a => a -> [a] -> Bool
member _ [] = False
member a (x:xs) = a == x || member a xs


(!?) :: [a] -> Int -> Maybe a
(!?) xs n = listToMaybe $ drop n xs


takeOut :: Int -> [a] -> ([a], [a])
takeOut n xs = (take n xs, drop (n + 1) xs)


shuffle :: (RandomGen g) => [a] -> Rand g [a]
shuffle [] = return []
shuffle xs = do
  n <-  getRandomR (0, length xs - 1)
  let (nth, xs') = extractNth xs n
  rest <- shuffle xs'
  return $ nth : rest 
  where
    extractNth xs' n = (xs' !! n, take n xs' ++ drop (n + 1) xs')
