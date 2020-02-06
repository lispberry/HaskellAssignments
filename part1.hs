combination :: Integer -> Integer -> Integer
combination n m = fact n `div` (fact (n - m) * fact m) where
  fact n = product [1..n]

gcd' :: Integer -> Integer -> Integer
gcd' m 0 = m
gcd' m n = gcd' n (m `mod` n)