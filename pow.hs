module Pow (pow) where
pow :: Int -> Integer -> Integer 
pow 0 x = 1
pow n x = x * pow (n-1) x