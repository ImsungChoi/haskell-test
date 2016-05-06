module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend s x y = f
  where f :: State
        f x'  | x == x'   = y
              | otherwise = s x' 

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

toInt :: Bool -> Int
toInt True = 1
toInt _    = 0

evalE :: State -> Expression -> Int
evalE s (Var x)       = s x
evalE _ (Val x)       = x
evalE s (Op e1 b e2)  = opHelper (evalE s e1) b (evalE s e2)
  where opHelper :: Int -> Bop -> Int -> Int
        opHelper x1 Plus x2   = x1 + x2
        opHelper x1 Minus x2  = x1 - x2
        opHelper x1 Times x2  = x1 * x2
        opHelper x1 Divide x2 = quot x1 x2
        opHelper x1 Gt x2     = toInt (x1 > x2)
        opHelper x1 Ge x2     = toInt (x1 >= x2)
        opHelper x1 Lt x2     = toInt (x1 < x2)
        opHelper x1 Le x2     = toInt (x1 <= x2)
        opHelper x1 Eql x2    = toInt (x1 == x2)

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s e)      = DAssign s e
desugar (If e s1 s2)      = DIf e (desugar s1) (desugar s2)
desugar (While e s)       = DWhile e (desugar s)
desugar (Sequence s1 s2)  = DSequence  (desugar s1) (desugar s2)
desugar (Skip)            = DSkip
desugar (Incr s)          = DAssign s (Op (Var s) Plus (Val 1))
desugar (For s1 e s2 s3)  = desugar (Sequence s1 (While e (Sequence s3 s2)))

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple s (DAssign x e)      = extend s x (evalE s e)
evalSimple s (DIf e d1 d2)      = if (evalE s e == 1) 
                                  then evalSimple s d1
                                  else evalSimple s d2
evalSimple s p@(DWhile e d)     = if (evalE s e == 1)
                                  then evalSimple (evalSimple s d) p
                                  else s
evalSimple s (DSequence d1 d2)  = evalSimple (evalSimple s d1) d2
evalSimple s (DSkip)            = s       

run :: State -> Statement -> State
run s1 s2 = evalSimple s1 (desugar s2)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]