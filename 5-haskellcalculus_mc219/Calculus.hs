module Calculus where
--
import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  fromInteger = Val . fromInteger
  negate (UnApp Neg a) = a
  negate a
    | a == 0        = a
    | otherwise     = UnApp Neg a

  (+) a b
    | a == (Val 0) = b
    | b == (Val 0) = a
    | otherwise    = BinApp Add a b

  (*) a b
    | a == 0 ||
      b == 0       = 0
    | a == 1       = b
    | b == 1       = a
    | otherwise    = BinApp Mul a b
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined

instance Fractional Exp where
  fromRational = Val . fromRational
  (/) a b
    | a == 0 ||
      b == 1        = a
    | otherwise     = BinApp Div a b
-- Leave the following one undefined...
  recip        = undefined

instance Floating Exp where
  sin     = UnApp Sin
  cos     = UnApp Cos
  log     = UnApp Log
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp x xs = fromJust $ lookup x xs

showExp :: Exp -> String
showExp (Val x)                = show x
showExp (Id x)                 = x
showExp (UnApp unOp ex1)       = lookUp unOp showUnOps ++ "(" ++ showExp ex1 ++ ")"
showExp (BinApp binOp ex1 ex2) = "(" ++ showExp ex1 ++ lookUp binOp showBinOps ++ showExp ex2 ++ ")"

showUnOps :: [(UnOp, String)]
showUnOps
  = [
    (Neg, "-"),
    (Sin, "sin"),
    (Cos, "cos"),
    (Log, "log")
    ]

showBinOps :: [(BinOp, String)]
showBinOps
  = [
    (Add, "+"),
    (Mul, "*"),
    (Div, "/")
    ]

-- Environment is a list of variable assignments so a lookup is
-- needed if a variable value is required called by id

eval :: Exp -> Env -> Double
eval (Val x) _                 = x
eval (Id x) env                = lookUp x env
eval (UnApp unOp ex) env       = lookUp unOp unOpsTable (eval ex env)
eval (BinApp binOp ex ex') env = lookUp binOp binOpsTable (eval ex env) (eval ex' env)


unOpsTable :: [(UnOp, Double -> Double)]
unOpsTable
  = [
    (Neg, negate),
    (Sin, sin),
    (Cos, cos),
    (Log, log)
    ]

binOpsTable :: [(BinOp, Double -> Double -> Double)]
binOpsTable
  = [
    (Add, (+)),
    (Div, (/)),
    (Mul, (*))
    ]

diff :: Exp -> String -> Exp
-- Constant
diff (Val c) _ = 0

-- Variable
diff (Id x) withRes
  | x == withRes = 1
  | otherwise    = 0

-- Sum Rule
diff (BinApp Add ex1 ex2) withRes
  = (diff ex1 withRes) + (diff ex2 withRes)

-- Product Rule
diff (BinApp Mul ex1 ex2) withRes
  = ex1 * (diff ex2 withRes) + (diff ex1 withRes) * ex2

-- Quotient Rule
diff (BinApp Div ex1 ex2) withRes
  =  ((diff ex1 withRes) * ex2 - ex1 * (diff ex2 withRes)) / (ex2 * ex2)

-- Negation
diff (UnApp Neg ex1) withRes
  = - (diff ex1 withRes)

-- Sin
diff (UnApp Sin ex1) withRes
  = (cos ex1) * (diff ex1 withRes)

-- Cos
diff (UnApp Cos ex1) withRes
  = - (sin ex1) * (diff ex1 withRes)

-- Log
diff (UnApp Log ex1) withRes
  = (diff ex1 withRes) / (ex1)

{-
diff :: Exp -> String -> Exp
-- Constant
diff (Val c) _ = Val 0.0

-- Variable
diff (Id x) withRes
  | x == withRes = Val 1.0
  | otherwise    = Val 0.0

-- Sum Rule
diff (BinApp Add ex1 ex2) withRes
  = BinApp Add (diff ex1 withRes) (diff ex2 withRes)

-- Product Rule
diff (BinApp Mul ex1 ex2) withRes
  = BinApp Add (BinApp Mul ex1 (diff ex2 withRes))
               (BinApp Mul (diff ex1 withRes) ex2)

-- Quotient Rule
diff (BinApp Div ex1 ex2) withRes
  = BinApp Div (numerator) (BinApp Mul ex2 ex2)
    where
      numerator
        = BinApp Add (BinApp Mul (diff ex1 withRes) ex2)
                     (UnApp Neg (BinApp Mul ex1 (diff ex2 withRes)))

-- Negation
diff (UnApp Neg ex1) withRes
  = UnApp Neg (diff ex1 withRes)

-- Sin
diff (UnApp Sin ex1) withRes
  = BinApp Mul (UnApp Cos ex1) (diff ex1 withRes)

-- Cos
diff (UnApp Cos ex1) withRes
  = UnApp Neg (BinApp Mul (UnApp Sin ex1) (diff ex1 withRes))

-- Log
diff (UnApp Log ex1) withRes
  = BinApp Div (diff ex1 withRes) (ex1)
-}


maclaurin :: Exp -> Double -> Int -> Double
maclaurin func val n
  = foldl1 (+) (take n (zipWith3 (\x y z -> ((flip eval) env (x * y)) * z) diffs pows facts))
  where
    diffs = iterate ((flip diff) "x") (func)
    facts = scanl (/) 1 (iterate (+1) 1) -- For some reason [1..] doesnt work here
    pows  = iterate ((*) (Id "z")) (Val 1.0)
    env   = [("z",val),("x",0)]

---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
