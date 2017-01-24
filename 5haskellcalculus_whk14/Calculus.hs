module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]


binOps :: [(BinOp, (Double -> Double -> Double))]
binOps = [(Add , (+)), (Mul , (*)), (Div , (/))]

unOps :: [(UnOp, (Double -> Double))]
unOps = [(Neg, ((-1)*)), (Sin, (sin)), (Cos, (cos)), (Log, (log))]


lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x list = z
        where z:zs = [b | (a, b) <- list, x==a]


eval :: Exp -> Env -> Double
eval (Val a) list        = a
eval (Id a) list         = lookUp a list
eval (BinApp x a b) list = (lookUp x binOps) (eval a list) (eval b list)
--eval (UnApp x a) list    = (lookUp x unOps) (eval a list)
eval (UnApp Neg a) list    = - ((eval a list))
eval (UnApp Sin a) list    = sin (eval a list)
eval (UnApp Cos a) list    = cos (eval a list)
eval (UnApp Log a) list    = log (eval a list)


diff :: Exp -> String -> Exp
diff (Val a) x = Val 0
diff (Id a) x
        | a == x    = Val 1
        | otherwise = Val 0
diff (UnApp Neg a) x    = UnApp Neg (diff a x)
diff (BinApp Add a b) x = BinApp Add (diff a x) (diff b x)
diff (BinApp Mul a b) x = BinApp Add (BinApp Mul a (diff b x)) 
                                     (BinApp Mul (diff a x) b)
diff (BinApp Div a b) x = BinApp Div top bottom
        where bottom    = BinApp Mul b b
              top       = BinApp Add (BinApp Mul (diff a x) b) 
                                     (UnApp Neg (BinApp Mul a (diff b x)))
diff (UnApp Sin a) x    = BinApp Mul (UnApp Cos a) (diff a x)
diff (UnApp Cos a) x    = UnApp Neg (BinApp Mul (UnApp Sin a) (diff a x))
diff (UnApp Log a) x    = BinApp Div (diff a x) a


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

powerN :: Double -> Int -> Double
powerN x 0 = 1
powerN x 1 = x
powerN x n = x * powerN x (n-1)

maclaurin :: Exp -> Double -> Int -> Double
maclaurin f pt n = maclaurin' f pt n 0
  where
    maclaurin' :: Exp -> Double -> Int -> Int -> Double
    maclaurin' f pt n t
      | t == n = 0
      | otherwise  = (top/bottom) + (maclaurin' (diff f "x") pt n (t+1))
        where
          top    = (eval f [("x",0)]) * (powerN pt t)
          bottom = fromIntegral (factorial t)


showExp :: Exp -> String
showExp (Val a)           = show a
showExp (Id a)            = a
showExp (BinApp opt a b)  = concat ["(", showExp a, op opt, showExp b, ")"]
                              where op x
                                      | x == Add = "+"
                                      | x == Mul = "*"
                                      | x == Div = "/"
--showExp (BinApp Mul a b)  = showExp a ++ "*" ++ showExp b
showExp (UnApp opt a)     = concat [op opt, showExp a, ")"]
                              where op x
                                      | x == Neg = "-("
                                      | x == Sin = "sin("
                                      | x == Cos = "cos("
                                      | x == Log = "log("

---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

----------------------------------------------------------------------
-- EXTENSION: Uncomment and complete these...

-- instance Num Exp where

-- instance Fractional Exp where

-- instance Floating Exp where

-- instance (Eq a, Num a) => Num (Maybe a) where

-- instance (Eq a, Fractional a) => Fractional (Maybe a) where

-- diff2 :: Exp -> String -> Maybe Exp

-- The following makes it much easier to input expressions, e.g. sin x, log(x*x) etc.

x, y :: Exp
x = Id "x"
y = Id "y"
