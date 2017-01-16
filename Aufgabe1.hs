import Data.Char
{-

  Michael Ion
  Matr.Nr.: 1005233

  Aufgabe 1-1:
	facInv m: startet die rekursive funktion facIter mit t=1
	facIter (t, m): hier überprüfen wir den wert m und schauen,
			ob m dasselbe ist wie die fakultät des aktuellen t;
			wenn es kleiner ist, setzen wir fort
			wenn größer, brechen wir ab und geben -1 zurück
	fac n: gibt die Fakultät von n zurück	
-}
facInv :: Integer -> Integer
facInv m
    | m <= 1 = (-1)
    | otherwise = facIter (1, m)

facIter :: (Integer, Integer) -> Integer
facIter (t, m)
    | m < (fac t) = -1
    | m > (fac t) = facIter ((t+1), m)
    | otherwise = t

fac :: Integer -> Integer
fac n
    | n == 0    = 1
    | otherwise = n * fac(n-1)

{-
  Aufgabe 1-2:
	extractDigits übernimmt einen String und parst einzelne Character: wenn ihr unicode wert 
	zwischen 48 und 57 (für Ziffern) ist, bleiben sie erhalten, sonst
	werden sie nicht übernommen				
-}
extractDigits :: String -> String
extractDigits [] = ""
extractDigits(x:xs)
	| (ord x >= 48) && ( ord x <= 57)  = [x] ++ extractDigits xs
	| otherwise  = "" ++ extractDigits xs

{-
  Aufgabe 1-3:
	Zuerst bilden wir einen String mir nur Ziffern. Dann übergeben wir an die rekursive 
	Funktion intConcat, die die aktuelle ziffer zu einem Integer castet (funktioniert, auch wenn
	die methode für hexadezimal vorgesehen ist) und mit vielfachem von 10 multipliziert (für den richtigen stellenwert),
	das ergebnis wird rekursiv addiert
-}

convert :: String -> Integer
convert a = intConcat (extractDigits a, 0)

intConcat :: (String, Integer) -> Integer
intConcat([], n) = n
intConcat(x:xs, n) = intConcat(xs,((toInteger (digitToInt x)) * (10^(length xs)) + n))

{-
  Aufgabe 1-4:
	findLeftMostPrime nimmt eine beliebige zeichenkette, filtert ziffern heraus und gibt
	die erste gefundene primzahl von links zurück mit der angegebenen länge 'len'
	
	> dafür verwenden  wir die gefilterte zeichenkette, die gewünschte länge und eine liste an primzahlen mit derselben länge
-}

findLeftMostPrime :: String -> Int -> Integer
findLeftMostPrime _ 0 = 0
findLeftMostPrime str len 
	| (length (extractDigits str)) < len = 0
	| otherwise = getPrime (convert str) (toInteger len) (primes (toInteger len))


{-
	getPrime bekommt die Ziffernkette und überprüft von links nach rechts, ob eine
	Primzahl der Länge len enthalten ist

   	mein ansatz funktioniert so:
	- zuerst wird über getLenDgts geschaut, ob die ersten "len" ziffern eine primzahl sind, falls ja, wird diese returned
	- sonst wird getPrime rekursiv aufgerufen mit der ziffernkette, wo die erste ziffer entfernt wurde
-}
getPrime :: Integer -> Integer -> [Integer] -> Integer
getPrime num len primelist
	| ((lenNum num) - len) < 0 = 0
	| (elem (getLenDgts num len) primelist) = getLenDgts num len
	| otherwise = getPrime (remFrstDgt num) len primelist

{-
	Es folgen Hilfsmethoden.

	Beispiel des rekursiven Aufrufs an der Zahl 3502:	
	3502 div 100 = 35	> ist 35 Primzahl? nein	
	3502 mod 3000 = 502
		
	502 div 10 = 50	> ist 50 Primzahl? nein	
	502 mod 500 = 02

	02 div 1 = 02 > gibt 0 zurück
-}

-- gibt die ersten "len" stellen einer zahl zurück
getLenDgts :: Integer -> Integer -> Integer
getLenDgts num len = (num `div` (10^((lenNum num)-len)) )

-- entfernt die erste Stelle einer Zahl und gibt das ergebnis zurück
remFrstDgt :: Integer -> Integer
remFrstDgt n = (n `mod`( (clearDigits n)))

-- gibt die eingabezahl zurück, wo alle stellen nach der 1. auf 0 gesetzt sind: z.b. 5321 -> 5000
clearDigits :: Integer -> Integer
clearDigits n = ((n `div` (multi n)) * (multi n))

-- gibt ein vielfaches von 10 in derselben länge wie das argument
multi :: Integer -> Integer
multi n = 10^((lenNum n)-1)

-- gibt die Länge eines integers zurück 
lenNum :: Integer -> Integer
lenNum n = toInteger ( 1 + floor (logBase 10 (fromIntegral n)))

-- gibt ein array primzahlen zurück, inspiriert von den vorlesungsfolien
sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve [y | y <- xs, mod y x > 0]
sieve [] = []

-- gibt nur primzahlen mit i stellen
primes :: Integer -> [Integer]
primes i = [k | k <- sieve [2..(10^i)], k > 10^(i-1)]

{-
  Aufgabe 1-5:
	findAll nimmt eine beliebige zeichenkette, filtert ziffern heraus und gibt
	alle gefundenen primzahlen von links nach rechts zurück mit der angegebenen länge 'len'
	
	> dafür verwenden wir, wie beim vorigen Beispiel, die gefilterte zeichenkette, die gewünschte länge und eine liste an primzahlen mit derselben länge
-}
findAllPrimes :: String -> Int -> [Integer]
findAllPrimes _ 0 = []
findAllPrimes str len 
	| (length (extractDigits str)) < len = []
	| otherwise = getPrimeList (convert str) (toInteger len) (primes (toInteger len)) []

-- funktioniert ähnlich wie getPrime, nur dass eine Liste von Primzahlen aufgebaut wird
getPrimeList :: Integer -> Integer -> [Integer] -> [Integer] -> [Integer]
getPrimeList num len primelist tempRes
	| num <= 0 = tempRes
	| ((lenNum num) - len) < 0 = tempRes
	| (elem (getLenDgts num len) primelist) = getPrimeList (remFrstDgt num) len primelist (tempRes ++ [(getLenDgts num len)])
	| otherwise = getPrimeList (remFrstDgt num) len primelist tempRes
