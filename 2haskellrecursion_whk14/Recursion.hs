module Recursion where

-- Precondition on all integers: they're all non-negative.

isPrime :: Int -> Bool
isPrime x
	| x < 2  	  = False 
	| x == 2 	  = True
	| even x && x > 2 = False
	| otherwise 	  = isPrime' 2
		where isPrime' n
			| x `mod` n == 0 			  = False
			| fromIntegral n >= sqrt (fromIntegral x) = True
			| otherwise 				  = isPrime' (n+1)

nextPrime :: Int -> Int
nextPrime x
	| x < 2 	= 2
	| isPrime (x+1) = x+1
	| otherwise 	= nextPrime (x+1)


modPow :: Int -> Int -> Int -> Int
-- Pre: 1 <= m <= sqrt(maxint)
modPow a b c
-- 	| even b = a ^ b `mod` c
--	| odd b  = ((a `mod` c) * (modPow a (b-1) c) `mod` c
	| b == 0 = 1 `mod` c
	| b == 1 = a `mod` c
	| even b = ((modPow a (b `div` 2) c) ^2) `mod` c
	| odd b  = ((a `mod` c) * (modPow a (b-1) c)) `mod` c


isCarmichael :: Int -> Bool
-- n <= sqrt (2^63-1) = 9223372036854775807
isCarmichael n
	| n < 2		= False
	| isPrime n	= False
	| otherwise	= testCar n (n-1)
		where testCar :: Int -> Int -> Bool
		      testCar n a
			| a < 2		    = True
			| modPow a n n /= a = False
			| otherwise	    = testCar n (a-1)
-- isPrime n includes n < 2 which are FALSE in isCarmichael


primeFactors :: Int -> [ Int ]
-- Pre: x >= 1
primeFactors x 
	| x == 1 = error "1 does not have prime factor."	
	| isPrime x = [1 , x]
	| otherwise = findPrime x 2
		where findPrime :: Int -> Int -> [ Int ]
		      findPrime x n
			| fromIntegral n > (sqrt (fromIntegral x) ) = [x]
			| x `mod` n == 0 = n : (findPrime (x `div` n) n)
			| otherwise 	 = findPrime x (n+1)
-- [x] on ln54 is to supply [] empty list to objects
-- ln 54: Max prime factor in middle of all factors has to be smaller than squareroot x


sumDigits :: Int -> Int
sumDigits x
	| x <= 9    = x
	| otherwise = x `mod` 10 + sumDigits (x `div` 10)


sumAllDigits :: [ Int ] -> Int
sumAllDigits [] 	  = 0
sumAllDigits [x]	  = sumDigits x
sumAllDigits ( x : xs )   = (sumDigits x) + (sumAllDigits xs)
--sumAllDigits ( x : xs )
--	| (x : xs) == [] = 0
--	| otherwise	 = (sumDigits x) + (sumAllDigits xs)

nextSmithNumber :: Int -> Int
nextSmithNumber x
	| x < 4 	        = 4
	| isSmithNumber (x+1)   = x+1
	| otherwise		= nextSmithNumber (x+1)
		where isSmithNumber :: Int -> Bool
		      isSmithNumber n
			| isPrime n 				       = False
			| sumDigits n == sumAllDigits (primeFactors n) = True
			| otherwise 				       = False
-- isPrime n includes n < 2 which are FALSE in isSmithNumber
-- how to shorten time to test?
