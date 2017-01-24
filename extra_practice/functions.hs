import Data.Char

-- post: returns a string comprised of the first character of each word.
abbreviate :: [String] -> String
abbreviate xs = map head xs  

-- post: returns the product of the given list of integers.
myProduct :: [Int] -> Int
{--using recursion...
myProduct [x] = x
myProduct (x:xs) = x * myProduct xs --}
--myProduct xs = foldl (*) 1 xs 
myProduct xs = foldl1 (*) xs

-- post: returns the numbers in the input list greater than or equal to the
--       given lower bound.
greaterThan :: [Int] -> Int -> [Int]
greaterThan [] n = []
greaterThan xs n = filter (>n) xs

-- post: returns the numbers in the input list which are exactly divisible by five.
divisibleBy5 :: [Int] -> [Int]
divisibleBy5 xs 
  = filter (candiv5) xs
    where
      candiv5 xs = (xs `mod` 5) == 0 

-- post: returns the uppercase representation of the given string.
upperString :: String -> String
upperString xs 
  = map (upChar) xs
    where
      upChar x = chr (ord x - ord 'a' + ord 'A')

-- post: returns the concatenated uppercase representation of the words in the input
--       list.
upperWords :: [String] -> String
upperWords xs = concatMap upperString xs

-- post: returns the coefficients for the derivative of the input coefficients.
deriv :: [Int] -> [Int]
deriv xs = drop 1 (zipWith (*) [0..] xs)
