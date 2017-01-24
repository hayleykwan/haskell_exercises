module Calculus2 where

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
lookUp x list = head [ b |(a,b) <- list, x == a]


eval :: Exp -> Env -> Double
eval (Val double) env = double
eval (Id string) env  = lookUp string env
eval (UnApp unop exp) env = (lookUp unop unOps) (eval exp env)
eval (BinApp binop e1 e2) env = (lookUp binop binOps) (eval e1 env) (eval e2 env)


diff :: Exp -> String -> Exp
diff (Val double) x = Val 0
diff (Id x) y
  | x == y    = Val 1
  | otherwise = Val 0
diff (UnApp unop exp) x
  | unop == Neg = UnApp Neg (diff exp x)
  | unop == Sin = BinApp Mul (UnApp Cos exp) (diff exp x)
  | unop == Cos = UnApp Neg (BinApp Mul (UnApp Sin exp) (diff exp x))
  | unop == Log = BinApp Div (diff exp x) exp
diff (BinApp binop e1 e2) x
  | binop == Add = BinApp Add (diff e1 x) (diff e2 x)
  | binop == Mul = BinApp Add (BinApp Mul e1 (diff e2 x))
                              (BinApp Mul (diff e1 x) e2)
  | binop == Div = BinApp Div (BinApp Add (ex1) (ex2))
                              (BinApp Mul e2 e2)
    where
      ex1 = BinApp Mul (diff e1 x) e2
      ex2 = UnApp Neg (BinApp Mul e1 (diff e2 x))


factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x-1)

mul :: Int -> Int
mul x = x * x

--use iterate with diff, scanl, zipWith3
-- iterate f x == [x, f x, f (f x), ...]
-- scanl (*) 1 [1,2,3,4,5] = [0!, 1!, 2!, 3!, 4!, 5!]
maclaurin :: Exp -> Double -> Int -> Double
maclaurin exp x n = sum (zipWith3 func diffZeros power fac)
  where
    func a b c = a * b / c
    diffZeros = map f differentials
    f e = eval e [("x", 0)]
    differentials = take n (iterate d exp)
    d e = diff e "x"
    power = [x^a | a <- [0,1..(n-1)]]
    fac = take n (map fromIntegral (scanl (*) 1 [1, 2..n]))


showExp :: Exp -> String
showExp (Val n) = show n
showExp (Id a) = a
showExp (UnApp unop a) = concat [ op unop, showExp a, ")"]
  where
    op unop
      | unop == Sin = "sin("
      | unop == Cos = "cos("
      | unop == Log = "log("
      | unop == Neg = "-("
showExp (BinApp binop e1 e2) = concat ["(", showExp e1, op binop, showExp e2, ")"]
  where
    op binop
      | binop == Add = "+"
      | binop == Mul = "*"
      | binop == Div = "/"
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
