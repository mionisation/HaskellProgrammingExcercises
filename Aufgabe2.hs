import Data.Char
{-

  Michael Ion
  Matr.Nr.: 1005233

  Aufgabe 2-1:
	fac n: gibt die Fakultät von n zurück
	facLst: Fakultäten in aufsteigender reihenfolge
	factsL: Fakultäten absteigend
-}

facLst :: Integer -> [Integer]
facLst n = reverse (factsL n)
--übergibt leere liste in der primzahlen appended werden
factsL :: Integer -> [Integer]
factsL n = facRec n []

-- appendet primzahlen absteigend, rekursiver aufruf mit fakultät-1
facRec :: Integer -> [Integer] -> [Integer]
facRec n lst
    | n < 0 = []
    | n == 0 = lst ++ [1]
    | otherwise = facRec (n-1) (lst ++ [fac n])

fac :: Integer -> Integer
fac n
    | n == 0    = 1
    | otherwise = n * fac(n-1)

{-
  Aufgabe 2-2:
  Nimmt aus einem String alle Zahlen und wandelt sie in eine Liste
-}

--fangen mit leerer liste an
extractNumerals :: String -> [String]
extractNumerals str = extractDigitTerm2 [] str

--wenn der string mit einer zahl anfängt, nehmen wir diese zahl heraus und rufen rekursiv mit dem string ohne dieser zahl auf
--wenn nicht, lösche den ersten buchstaben bis man zu einer zahl kommt
extractDigitTerm2 :: [String] -> String -> [String]
extractDigitTerm2 lst [] = lst
extractDigitTerm2 lst str
	| beginsWithDigit(str)  =  extractDigitTerm2 (lst ++ [extractDigitTerm(str)] ) (extractNonDigitTerm(str))
	| otherwise  = subtractFirstLetter lst str
--löscht den ersten buchstaben aus einem string
subtractFirstLetter :: [String] -> String -> [String]
subtractFirstLetter lst (x:xs) = extractDigitTerm2 lst xs
--true falls string mit ziffer beginnt
beginsWithDigit :: String -> Bool
beginsWithDigit (x:xs) = (ord x >= 48) && ( ord x <= 57)

-- ganze zahl am anfang des strings wird herausgenommen
extractDigitTerm :: String -> String
extractDigitTerm [] = ""
extractDigitTerm(x:xs)
	| (ord x >= 48) && ( ord x <= 57)  = [x] ++ extractDigitTerm xs
	| otherwise  = ""
-- zahl am anfang des strings wird weggeworfen
extractNonDigitTerm :: String -> String
extractNonDigitTerm [] = ""
extractNonDigitTerm(x:xs)
	| (ord x >= 48) && ( ord x <= 57)  = extractNonDigitTerm xs
	| otherwise  = (x:xs)

{-
  Aufgabe 2-3:
  Überprüfung ob etwas die potenz von 2 ist
-}

--erster teil: rekursiver aufruf zum Überprüfen, wie oft durch 2 teilbar
isPowOf2 :: Int -> (Bool,Int)
isPowOf2 zahl 
	| zahl <= 0 = (False, -1)
	| otherwise = powOf2Rec zahl 0

powOf2Rec :: Int -> Int -> (Bool,Int)
powOf2Rec zahl round
	| zahl == 1 = (True, round)
	| mod zahl 2 == 0 = powOf2Rec (zahl `div` 2) (round + 1)
	| otherwise = (False, -1)

--hilfsfunktionen für den zweiten teil, löscht den bool teil aus funktion darüber
isPowOf2simple :: Int -> Int
isPowOf2simple a = returnSecond (isPowOf2 a)

returnSecond :: (Bool, Int) -> Int
returnSecond (b, i) = i

-- string liste wird in liste der zweier potenzen umgewandelt
sL2pO2 :: [String] -> [Int]
sL2pO2 lst = stringListParser lst []

-- geht rekursiv die liste durch, konvertiert die strings zu ints und berechnet die zweierpotenz, erstellt eine neue liste damit 
stringListParser :: [String] -> [Int] -> [Int]
stringListParser (x:xs) intList = stringListParser (xs) (intList ++ [isPowOf2simple.fromIntegral.convert$x])
stringListParser [] intList = intList

-- wandelt einen string zu einem integer
convert :: String -> Integer
convert a = intConcat ( a, 0)
-- wenn ein anderes zeichen gefunden wird, ist das ergebnis -1, ansonsten konstruiere den integer aus den einzelnen chars
intConcat :: (String, Integer) -> Integer
intConcat([], n) = n
intConcat(x:xs, n)
		| (ord x >= 48) && ( ord x <= 57) = intConcat(xs,((toInteger (digitToInt x)) * (10^(length xs)) + n))
		| otherwise = (-1) 

{-
  Aufgabe 2-4:  

	zum Testen nehme ich einfache funktionen, die die argumente miteinander addieren:
	Testbeispiele:

	uncurry3 simpleCurried3Addition (1,2,3)
	6
	uncurry4 simpleCurried4Addition (1,2,3,4)
	10

	curry3 simpleUncurried3Addition 1 2 3 
	6	
	curry4 simpleUncurried4Addition 1 2 3 4
	10

-}

--uncurried funktionen zum testen der "curry" funktionale
simpleUncurried3Addition :: (Integer, Integer, Integer) -> Integer
simpleUncurried3Addition (a, b, c) = a + b + c

simpleUncurried4Addition :: (Integer, Integer, Integer, Integer) -> Integer
simpleUncurried4Addition (a, b, c, d) = a + b + c + d

--Curry funktionale
curry3 :: ((a,b,c) -> d) -> a -> b -> c -> d
curry3 f x y z = f ( x, y, z)

curry4 :: ((a,b,c,d) -> e) -> a -> b -> c -> d -> e
curry4 f x y z r = f(x, y, z, r)


--curried funktionen zum testen der "uncurry" funktionale
simpleCurried3Addition :: Integer -> Integer -> Integer -> Integer
simpleCurried3Addition a b c = a + b + c

simpleCurried4Addition :: Integer -> Integer -> Integer -> Integer -> Integer
simpleCurried4Addition a b c d = a + b + c + d

--Uncurry funktionale
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 g (x,y,z) = g x y z

uncurry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
uncurry4 g (x,y,z,r) = g x y z r
