BEISPIEL 1

> data Digit = Zero | One | Two deriving (Eq,Enum,Show)
> type Digits = [Digit]

Pos fuer Positive
Neg fuer Negative

> data Sign = Pos | Neg deriving (Eq,Show)
> newtype Numeral = Num (Sign,Digits) deriving (Eq,Show)

Canonize entfernt ueberschuessige Zeros am Anfang einer Reihe

> canonize :: Numeral -> Numeral

wandle um lauf Definitionen: leere Listen nicht gueltig, neg zero zu pos zero, 

> canonize (Num (Pos,[])) = error "Keine gueltige Zahldarstellung"
> canonize (Num (Neg,[])) = error "Keine gueltige Zahldarstellung"
> canonize (Num (Neg, [Zero])) = (Num (Pos, [Zero]))
> canonize (Num (s, d)) = Num (s, rmZeros(d))

entfernt prefix zeros

> rmZeros :: Digits -> Digits
> rmZeros (x:xs)
>	| x == Zero = rmZeros(xs)
>	| otherwise = ([x] ++ xs)
> rmZeros [] = [Zero]

Wandelt Numerale von Base 3 ins Zehnersystem

> num2int :: Numeral -> Integer
> num2int (Num (s, d)) = calculateInt s d 0

Rekursiver Aufruf um die einzelnen Stellen der Base 3 Zahl zu potenzieren

> calculateInt :: Sign -> [Digit] -> Integer -> Integer
> calculateInt sign (x:xs) currentNum = calculateInt sign xs (currentNum + getIntFromDigit(x)* 3^(length(xs)))
> calculateInt sign [] currentNum = currentNum * getIntFromSign(sign)

Hilfsfunktionen um die Ziffern bzw Vorzeichen in Integer zu wandeln

> getIntFromDigit :: Digit -> Integer
> getIntFromDigit Zero = 0
> getIntFromDigit One = 1
> getIntFromDigit Two = 2
> getIntFromDigit _ = error "Invalid Argument"

> getIntFromSign :: Sign -> Integer
> getIntFromSign Pos = 1
> getIntFromSign Neg = -1
> getIntFromSign _ = error "Invalid Argument"

Wandelt einen Integer in ein Numeral um, zwei separate hilfsfunktionen für vorzeichen und zahlen jeweils

> int2num :: Integer -> Numeral
> int2num 0 = (Num (Pos, [Zero]))
> int2num integ = (Num (getSignFromInt(integ), getDigitsFromInt(abs(integ))))

hilfsfunktion um das vorzeichen herauszufinden

> getSignFromInt :: Integer -> Sign
> getSignFromInt num
>		| num < 0 = Neg
>		| otherwise = Pos

starte rekursiven aufruf

> getDigitsFromInt :: Integer -> Digits
> getDigitsFromInt num = digitsToBaseThree num []

Formel um 10er system in 3er System umzuwandeln: rekursiv durch 3 dividieren und den mod - rest an die liste anbinden

> digitsToBaseThree :: Integer -> [Digit] -> Digits
> digitsToBaseThree 0 digits = digits
> digitsToBaseThree integ digits =  digitsToBaseThree (integ `div` 3) (getDigitFromInt( (integ `mod` 3) ) ++ digits)

Einzelne Ziffer in Digits

> getDigitFromInt :: Integer -> [Digit]
> getDigitFromInt 0 = [Zero]
> getDigitFromInt 1 = [One]
> getDigitFromInt 2 = [Two]
> getDigitFromInt _ = error "Invalid Argument"

BEISPIEL 2
Make numerals by ONE smaller oder bigger

input kanonisieren

> inc :: Numeral -> Numeral
> inc num = inc2 (canonize num)

Specialfall falls -1 inkrementiert wird
man verwendet die inverse liste um den Zugriff leichter zu machen

> inc2 :: Numeral -> Numeral
> inc2 (Num (Neg, [One])) = (Num (Pos, [Zero]))
> inc2 (Num(Pos, d)) =  canonize ( Num( Pos, (incrementDigits (reverse(d)) [] )))
> inc2 (Num(Neg, d)) = canonize ( Num( Neg, (decrementDigits (reverse(d)) [] )))

input kanonisieren

> dec :: Numeral -> Numeral
> dec num = dec2 (canonize num)

Specialfall falls +0 decrementiert wird
man verwendet die inverse liste um den Zugriff leichter zu machen

> dec2 :: Numeral -> Numeral
> dec2 (Num (Pos, [Zero])) = Num(Neg, [One])
> dec2 (Num(Neg, d)) = canonize ( Num( Neg, (incrementDigits (reverse(d)) [] )))
> dec2 (Num(Pos, d)) = canonize ( Num( Pos, (decrementDigits (reverse(d)) [] )))

Addiert eins zu der Digitliste
Falls die letzte Stelle Zero oder One, füge einfach eins hinzu und gib das ergebnis zurÜck (nachdem zurück invertiert wurde)
Falls Two, wird einerstelle Zero und rekursiv weiter gegeben, um nächste Stelle zu inkrementieren

> incrementDigits :: Digits -> [Digit] -> Digits
> incrementDigits [] last = [One] ++ last
> incrementDigits (x:xs) last
>	| x == Zero = (reverse xs) ++ [One] ++ last
>	| x == One = (reverse xs) ++ [Two] ++ last
>	| x == Two = incrementDigits xs ([Zero] ++ last)

Ähnlich zu increment: Falls letzte Stelle Two oder One, einfach zu nächst kleinerem
Falls Zero, füge eine Zwei an die aktuelle Stelle und Überprüfe die restlichen Stellen rekursiv

> decrementDigits :: Digits -> [Digit] -> Digits
> decrementDigits [] last = [Zero] ++ last
> decrementDigits (x:xs) last
>	| x == Two = (reverse xs) ++ [One] ++ last
>	| x == One = (reverse xs) ++ [Zero] ++ last
>	| x == Zero = decrementDigits xs ([Two] ++ last)

BEISPIEL 3
Addieren und Multiplizieren mit Numerals
Wir verwenden einfach dec und inc und führen sie entsprechend oft durch!

> numAdd :: Numeral -> Numeral -> Numeral
> numAdd x y = addTimes x (num2int(y))

Rekursiver aufruf der sooft addiert wie im zweiten argument (integer) angegeben

> addTimes :: Numeral -> Integer -> Numeral
> addTimes num times
>	| times == 0 = num
>	| times < 0 = addTimes (dec num) (times+1)
>	| times > 0 = addTimes (inc num) (times-1)

> numMult :: Numeral -> Numeral -> Numeral
> numMult (Num (Pos, [Zero])) _ = (Num (Pos, [Zero]))
> numMult _ (Num (Pos, [Zero])) = (Num (Pos, [Zero]))
> numMult x y = Num( (createMultSign x y), (getDigitsFromNum(numMultHelper x y)))

> getDigitsFromNum :: Numeral -> Digits
> getDigitsFromNum (Num(_, d)) = d

> createMultSign :: Numeral -> Numeral -> Sign
> createMultSign n1 n2
>	| (num2int(n1) * num2int(n2)) > 0 = Pos
>	| (num2int(n1) * num2int(n2)) < 0 = Neg


> numMultHelper :: Numeral -> Numeral -> Numeral
> numMultHelper (Num(_, d)) y = multTimes (Num(Pos, d)) (num2int((Num(Pos, d)))) (abs(num2int(y)))

Rekursiver aufruf der sooft addTimes aufruft wie im zweiten argument (integer) angegeben

> multTimes :: Numeral -> Integer -> Integer -> Numeral
> multTimes num orignum times
>	| times == 1 = num
>	| otherwise = multTimes (addTimes num orignum) orignum (times-1)

BEISPIEL 4

Curry und uncurry flips

> curryFlip :: (((a,b) -> c) -> (b -> (a -> c)))
> curryFlip f x y = f (y,x)

> uncurryFlip :: (((a -> (b -> c)) -> ((b,a) -> c)))
> uncurryFlip f (x,y) = f y x

> pairFlip :: (((a,b) -> c) -> ((b,a) -> c))
> pairFlip f (b, a) = f (a,b)

curried funktion zum testen der "uncurryFlip" funktionale

> divideCurry :: Float -> Float -> Float
> divideCurry a b = a/b

uncurried funktion zum testen der "curryFlip" funktionale und von pairFlip

> divideUncurry :: (Float, Float) -> Float
> divideUncurry (a,b) = a/b

Beispiele zum Testen der Funktionale:

alle Flip Befehle müssen folglich das Inverse von divideCurry berechnen:
Aufrufe:

divideUncurry (3, 1)
3.0

pairFlip divideUncurry (3, 1)
0.3333333

curryFlip divideUncurry 3 1
0.3333333

uncurryFlip divideCurry (3,1)
0.3333333

