{-# LANGUAGE BangPatterns #-}
-- Exercise 1
isPrime :: Int -> Bool
isPrime n
  | n < 2     = False
  | n == 2    = True
  | even n    = False
  | otherwise = null [ x | x <- [2 .. floor $ sqrt $ fromIntegral n], n `mod` x == 0 ]

goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n =[(p, q) | p <- [2 .. n], isPrime p, q <- [p .. n], isPrime q, p + q == n] 

--Exercise 2
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs =[ (x, y) | x <- xs, y <- xs, x < y, gcd x y == 1]

--Excercise 3 
sieve [] = []
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime2 :: Int -> Bool
isPrime2 n = n `elem` primesTo n

--Excercise 4
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b =
  [ [ sum [ a !! i !! k * b !! k !! j | k <- [0 .. p-1] ]
    | j <- [0 .. n-1] ]
  | i <- [0 .. m-1] ]
  where
    m = length a
    p = length (head a)
    n = length (head b)


--Excercise 5
permutations :: Eq a => Int -> [a] -> [[a]]
permutations 0 _  = [[]]
permutations _ [] = []
permutations k xs = [ x:ys | (x, rest) <- picks xs, ys <- permutations (k-1) rest]
  where picks [] = []
        picks (y:ys) = (y, ys) : [(z, y:zs) | (z, zs) <- picks ys]

--Excercise 6
--a)
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | x > y     = y : merge (x:xs) ys
  | otherwise = x : merge xs ys

--b)
hamming :: [Integer]
hamming = 1 : merge (map (2*) hamming)
                    (merge (map (3*) hamming)
                           (map (5*) hamming))


--Excercise 7
power :: Int -> Int -> Int
power b e = go b e 1
  where
    go _ 0 !acc = acc
    go base e !acc =
      go base (e - 1) (acc * base)

--Excercise 8
listMaxSeq :: [Int] -> Int
listMaxBang :: [Int] -> Int
--using seq
listMaxSeq (x:xs) = go x xs
  where
    go acc [] = acc
    go acc (y:ys) =
      let newAcc = max acc y
      in newAcc `seq` go newAcc ys
--using bangpatterns
listMaxBang (x:xs) = go x xs
  where
    go !acc [] = acc
    go !acc (y:ys) =
      go (max acc y) ys


--Excercise 9
primes :: [Int]
primes = sieve [2..]

isPrimeInfinite :: Int -> Bool
isPrimeInfinite n = n `elem` takeWhile (<= n) primes


--Excercise 10
--a)
mean :: [Double] -> Double
mean xs = total / fromIntegral count
  where
    (total, count) = go xs (0, 0)
    go [] (s, c) = (s, c)
    go (y:ys) (s, c) =
      go ys (s + y, c + 1)
--b)
meanStrict :: [Double] -> Double
meanStrict xs = total / fromIntegral count
  where
    (total, count) = go xs (0, 0)
    go [] (!s, !c) = (s, c)
    go (y:ys) (!s, !c) =
      let !s' = s + y
          !c' = c + 1
      in go ys (s', c')  
--c)
meanVar :: [Double] -> (Double, Double)
meanVar xs = (mu, variance)
  where
    (s, s2, n) = go xs (0, 0, 0)
    mu = s / fromIntegral n
    variance = (s2 / fromIntegral n) - mu * mu

    go [] (!sum1, !sum2, !count) = (sum1, sum2, count)
    go (y:ys) (!sum1, !sum2, !count) =
      let !sum1' = sum1 + y
          !sum2' = sum2 + y*y
          !count' = count + 1
      in go ys (sum1', sum2', count') 
