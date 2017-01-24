import Data.Char

-- post: returns a string comprised of the first character of each word.
abbreviate :: [String] -> String
abbreviate words = map head words

-- post: returns the product of the given list of integers.
myProduct :: [Int] -> Int
myProduct nums = foldl1 (*) nums

-- post: returns the numbers in the input list greater than or equal to the
--       given lower bound.
greaterThan :: [Int] -> Int -> [Int]
greaterThan nums limit = filter (>limit) nums

-- post: returns the numbers in the input list which are exactly divisible by five.
divisibleBy5 :: [Int] -> [Int]
divisibleBy5 nums = filter (divBy5) nums
  where divBy5 x = x `mod` 5 == 0

-- post: returns the uppercase representation of the given string.
upperString :: String -> String
upperString string = map upChar string
  where upChar x = chr (ord x - ord 'a' + ord 'A')

-- post: returns the concatenated uppercase representation of the words in the input
--       list.
upperWords :: [String] -> String
upperWords words = concat (map upperString words)

-- post: returns the coefficients for the derivative of the input coefficients.
deriv :: [Int] -> [Int]
deriv nums = drop 1 (zipWith (*) nums [0,1..(length nums)-1])
