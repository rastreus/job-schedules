{- Copyright © 2015 Russell Dillin

sched is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

sched is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with sched. If not, see <http://www.gnu.org/licenses/>. -}

getGreatestDiff :: [(Int, Int)] -> (Int, Int) -> Int
getGreatestDiff ys t = getGreatestDiff' ys t 0

getGreatestDiff' :: [(Int, Int)] -> (Int, Int) -> Int -> Int
getGreatestDiff' [] t diff = diff
getGreatestDiff' ys t diff
    | sub > diff = getGreatestDiff' (tail ys) t sub
    | otherwise  = getGreatestDiff' (tail ys) t diff
    where
      sub = snd t - snd (head ys)

diffForBest :: [(Int, Int)] -> Int -> (Int, Int) -> [(Int, Int)]
diffForBest ys d t = diffForBest' ys d t []

diffForBest' :: [(Int, Int)] -> Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
diffForBest' [] d t zs = zs
diffForBest' ys d t zs
    | h > f && (h - f) == diff = diffForBest' (tail ys) d t (t : zs)
    | otherwise                = diffForBest' (tail ys) d t (head ys : zs)
    where
      f    = snd (head ys)
      h    = snd t
      diff = getGreatestDiff ys t

keepTheBest :: [(Int, Int)] -> Int -> (Int, Int) -> [(Int, Int)]
keepTheBest ys d t = keepTheBest' ys d t []

keepTheBest' :: [(Int, Int)] -> Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
keepTheBest' [] d t zs
    | length zs == d = diffForBest zs d t
    | otherwise      = t : zs
keepTheBest' ys d t zs
    | h > f && e <= g && g <= count = keepTheBest' (tail ys) d t zs
    | otherwise                     = keepTheBest' (tail ys) d t (head ys : zs)
    where
      e     = fst (head ys)
      f     = snd (head ys)
      g     = fst t
      h     = snd t
      count = countDeadlines ys g

buildBestOrder :: [(Int, Int)] -> Int -> (Int, Int) -> [(Int, Int)]
buildBestOrder ys d t = buildBestOrder' ys d t []

buildBestOrder' :: [(Int, Int)] -> Int -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
buildBestOrder' [] d t zs
    | g /= count = t : zs
    | otherwise  = zs
    where
      g     = fst t
      count = countDeadlines zs g
buildBestOrder' ys d t zs
    | h > f && e == g && g == count = buildBestOrder' (tail ys) d t zs
    | otherwise                     = buildBestOrder' (tail ys) d t (head ys : zs)
    where
      e     = fst (head ys)
      f     = snd (head ys)
      g     = fst t
      h     = snd t
      count = countDeadlines ys g

bestOrders :: [(Int, Int)] -> Int -> (Int, Int) -> [(Int, Int)]
bestOrders [] d t = [t]
bestOrders ys d t
    | l  > 0 && l < d = buildBestOrder ys d t
    | l == d          = keepTheBest ys d t
    where
      l = length ys

getBestOrders :: [(Int, Int)] -> Int -> [(Int, Int)]
getBestOrders xs d = getBestOrders' xs d []

getBestOrders' :: [(Int, Int)] -> Int -> [(Int, Int)] -> [(Int, Int)]
getBestOrders' [] d ys     = ys
getBestOrders' (x:xs) d ys = getBestOrders' xs d (bestOrders ys d x)

countDeadlines :: [(Int, Int)] -> Int -> Int
countDeadlines xs d = countDeadlines' xs d 0

countDeadlines' :: [(Int, Int)] -> Int -> Int -> Int
countDeadlines' [] d n     = n
countDeadlines' (x:xs) d n
    | y == d               = countDeadlines' xs d (n + 1)
    | otherwise            = countDeadlines' xs d n
    where
      y = fst x

getProfit :: [(Int, Int)] -> Int
getProfit xs = getProfit' xs 0

getProfit' :: [(Int, Int)] -> Int -> Int
getProfit' xs n = foldl (\ n x -> n + snd x) n xs

getDeadline :: [(Int, Int)] -> Int
getDeadline xs = getDeadline' xs 0

getDeadline' :: [(Int, Int)] -> Int -> Int
getDeadline' [] n     = n
getDeadline' (x:xs) n
    | y > n           = getDeadline' xs y
    | otherwise       = getDeadline' xs n
    where
      y = fst x

maxProfit :: [(Int, Int)] -> Int
maxProfit xs = getProfit $ getBestOrders xs (getDeadline xs)
