{-

  Michael Ion
  Matr.Nr.: 1005233

  Aufgabe 6 - 1	
  Stack LIFO principle
-}

data Stack a = Stk [a] | NoStk deriving (Eq,Show)
--creates empty stack
empty :: (Eq a,Show a) => Stack a
empty = Stk []
--checks if empty
isEmpty :: (Eq a,Show a) => Stack a -> Bool
isEmpty st
	| st == Stk [] = True
	| otherwise = False
--returns top value
top1 :: (Eq a,Show a) => Stack a -> a
top1 NoStk = error "Invalid Argument"
top1 (Stk []) = error "Invalid Argument"
top1 (Stk (l:ls) ) = l
--returns top value as Just
top2    :: (Eq a,Show a) => Stack a -> Maybe a
top2 NoStk = Nothing
top2 (Stk []) = Nothing
top2 (Stk (l:ls) ) = Just l
--returns stack minus top value
pop     :: (Eq a,Show a) => Stack a -> Stack a
pop (NoStk) = NoStk
pop (Stk []) = NoStk
pop (Stk (l:ls) ) = Stk ls
--returns stack plus new value
push :: (Eq a,Show a) => a -> Stack a -> Stack a
push a NoStk = NoStk
push a (Stk la) = Stk  ([a] ++ la)

{-
  Aufgabe 6 - 2	
  Rechnen mit NUMS gemäß vordefinierter Expressions
-}

data Digit      = Zero | One | Two deriving (Eq,Enum,Show)
type Digits     = [Digit]
data Sign       = Pos | Neg  deriving (Eq,Show) -- Pos fuer Positive,
						-- Neg fuer Negative
newtype Numeral = Num (Sign,Digits) deriving (Eq,Show)
data Operator   = Plus | Times | Minus deriving (Eq,Show)
						-- Plus fuer Addition,
						-- Times fuer Multiplikation,
						-- Minus fuer Subtraktion
data Variable   = A | B | C deriving (Eq,Show)
data Expression = Cst Numeral
		| Var Variable
		| Exp Expression Expression Operator deriving (Eq,Show)
type State      = Variable -> Numeral  -- Total definiert, gueltig und kanonisch

e1 = Exp (Cst (Num (Pos,[One,Zero,Two]))) (Var B) Plus

e2 = Exp (Exp (Cst (Num (Neg,[One,Zero,Two]))) (Var B) Plus) (Var C) Minus -- ((-11)+B)-C)

e3 = Exp (Exp (Cst (Num (Pos,[One,Zero,Two]))) (Var B) Minus) (Var C) Times

e4 = Cst (Num (Neg,[One,Zero,Two]))

e5 = Exp e2 e3 Plus

s :: State
s A = Num (Pos,[Zero]) -- entspricht s(A) = 0
s B = Num (Neg,[One,Zero,One]) -- s(B) = -10
s C = Num (Pos,[One,One,One]) -- s(C) = 13

eval :: Expression -> State -> Integer
eval (Cst num) s = num2int num
eval (Var var) s = num2int.s$var 
eval (Exp exp1 exp2 op) s
	| op == Minus = ((eval (exp1) s) - (eval (exp2) s))
	| op == Plus = ((eval (exp1) s) + (eval (exp2) s))
	| op == Times = ((eval (exp1) s) * (eval (exp2) s))



{-HILFSFUNKTIONEN ALT-}

--Wandelt Numerale von Base 3 ins Zehnersystem

num2int :: Numeral -> Integer
num2int (Num (s, d)) = calculateInt s d 0

--Rekursiver Aufruf um die einzelnen Stellen der Base 3 Zahl zu potenzieren

calculateInt :: Sign -> [Digit] -> Integer -> Integer
calculateInt sign (x:xs) currentNum = calculateInt sign xs (currentNum + getIntFromDigit(x)* 3^(length(xs)))
calculateInt sign [] currentNum = currentNum * getIntFromSign(sign)

--Hilfsfunktionen um die Ziffern bzw Vorzeichen in Integer zu wandeln

getIntFromDigit :: Digit -> Integer
getIntFromDigit Zero = 0
getIntFromDigit One = 1
getIntFromDigit Two = 2
getIntFromDigit _ = error "Invalid Argument"

getIntFromSign :: Sign -> Integer
getIntFromSign Pos = 1
getIntFromSign Neg = -1
getIntFromSign _ = error "Invalid Argument"

{-
  Aufgabe 6 - 3	
  Rechnen mit NUMS gemäß vordefinierter Expressions
-}

data CVO = Cop Numeral -- Konstantenoperand
		| Vop Char -- Variablenoperand
		| OpPlus   -- Operator fuer Addition
		| OpTimes  -- Operator fuer Multiplikation
		| OpMinus  -- Operator fuer Subtraktion
			deriving (Eq,Show)
type Expr = [CVO]
type State2 = Char -> Numeral -- Total definiert, Numeralwert
			      -- gueltig und kanonisch
{-
Postfix evaluierung der rechenanweisungen wie in Expr [CVO] angegeben

1. baue einen stack mit operatoren, einen mit zahlen
2. rechne alles rekursiv aus: pop die letzten zwei werte sowie letzten operator usw.
-}

--there is only one value, the end result, in the stack after call of build stacks
eval2 :: Expr -> State2 -> Integer
eval2 e s = top1(buildStacks s empty e)

-- recognizes if value is numeric or var or whatevs
pushTheInt :: State2 -> CVO -> Stack Integer -> Stack Integer
pushTheInt s (Cop n) st = push (num2int n) st 
pushTheInt s (Vop n) st = push (num2int (s2 n)) st 

--builds a stack of numeric values from constand variables; if encouters symbol then resolve calculation, update stack, continue...
buildStacks :: State2 -> Stack Integer -> Expr -> Stack Integer
buildStacks s valS [] = valS
buildStacks s valS (x:xs)
	| (x == OpPlus) = buildStacks s (push ((top1 (pop valS)) + (top1 valS)) (pop (pop valS)) ) xs
	| (x == OpTimes) = buildStacks s (push ((top1 (pop valS)) * (top1 valS)) (pop (pop valS)) ) xs
	| (x == OpMinus) = buildStacks s (push ((top1 (pop valS)) - (top1 valS)) (pop (pop valS)) ) xs
	| otherwise = buildStacks s (pushTheInt s x valS) xs

s2 :: State2
s2 'A' = Num (Pos,[Zero])
-- entspricht s2('A') = 0
s2 'B' = Num (Neg,[One,Zero,One]) -- s2('B') = -10
s2 'C' = Num (Pos,[One,One,One]) -- s2('C') = 13
s2 'V' = Num (Pos,[Zero,One,One]) -- s2('V') = 4
s2 'Z' = Num (Pos,[Zero,Zero,Two]) -- s2('Z') = 2
s2 'D' = Num (Pos,[Zero,One,Zero]) -- s2('D') = 3
s2 'F' = Num (Pos,[Zero,One,Two]) -- s2('F') = 5
s2 'E' = Num (Pos,[Zero,Zero,One]) -- s2('E') = 1
s2 'S' = Num (Pos,[Zero,Two,One]) -- s2('S') = 7
s2 _ = Num (Pos,[One])
e = [Vop 'A',Vop 'B',OpMinus,Vop 'C',OpPlus, Vop 'A', Vop 'B', Vop 'C', OpMinus, OpPlus, OpTimes]
eal = [Vop 'V',Vop 'F',Vop 'S',Vop 'Z',OpPlus,OpMinus,OpTimes] -- = -16

