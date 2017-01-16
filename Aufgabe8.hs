--Aufgabe 1: Lifestyle Events ---

data GeladenerGast = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T deriving (Eq,Ord,Enum,Show)
type Schickeria = [GeladenerGast] -- Aufsteigend geordnet
type Adabeis = [GeladenerGast] -- Aufsteigend geordnet
type Nidabeis = [GeladenerGast] -- Aufsteigend geordnet
type NimmtTeil = GeladenerGast -> Bool -- Total definiert
type Kennt = GeladenerGast -> GeladenerGast -> Bool -- Total definiert

gastList = [ A , B , C , D , E , F , G , H , I , J , K , L , M , N , O , P , Q , R , S , T ]
--gastList =  [ A , B , C , D , E]

-- Adabei: kennt jedes Mitglied der Schickeria, möglicherweise andere Adabeis, möglicherweise sich selbst
-- Schickeria: kennt alle anwesenden Mitglieder der Schickeria, einschließlich sich selbst, aber keinen einzigen Adabei. --
-- Nidabei: Ist eingeladen, aber erscheint nicht
--erstelle erst die liste der schickeria: 


-- check if some schickeria contained in nimmtTeil
istSchickeriaEvent :: NimmtTeil -> Kennt -> Bool
istSchickeriaEvent nT kT
	| length (schickeria nT kT) > 0 = True
	| otherwise = False

-- check if no adabeis in nimmtTeil
istSuperSchick :: NimmtTeil -> Kennt -> Bool
istSuperSchick nT kT
	| length (adabeis nT kT) == 0 = True
	| otherwise = False

-- check if no schickeria in nimmtTeil
istVollProllig :: NimmtTeil -> Kennt -> Bool
istVollProllig nT kT
	| length (schickeria nT kT) == 0 = True
	| otherwise = False

--erst alle aus nimmt teil, dann erstelle durchschnitt aus allen "kennt"
-- schachtelung: erst nimm alle gaeste die erkannt werden, dann zÄhle ob es die anzahl der anwesenden ist
schickeria :: NimmtTeil -> Kennt -> Schickeria
schickeria nT kT = nub [ t | t <- [gekannter | gekannter <- (dabeiLeute nT kT), kennend <- (dabeiLeute nT kT), (k kennend gekannter == True)], length (dabeiLeute nT kT) == count t [gekannter | gekannter <- (dabeiLeute nT kT), kennend <- (dabeiLeute nT kT), (k kennend gekannter == True)]]

--alle aus nimmt teil minus schickeria
adabeis :: NimmtTeil -> Kennt -> Adabeis
adabeis nT kT = [ n | n <- (dabeiLeute nT kT), elem n (schickeria nT kT) == False]

--alle aus nimmtTeil = false
nidabeis :: NimmtTeil -> Kennt -> Nidabeis
nidabeis nimmtTeil kennt = [n | n <- gastList, (nimmtTeil n) == False]

--alle die dabei sind
dabeiLeute :: NimmtTeil -> Kennt -> [GeladenerGast]
dabeiLeute nT kT = filter nT gastList

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

-- make some test data


n :: NimmtTeil
n A = True
n B = False
n C = True
n D = True
n E = True
-- D, E, schickeria, wird von allen gekannt
-- A, B, C pöbel , nicht von schickeria gekannt, untereinenander vielleicht
k :: Kennt
k A E = True
k B E = True
k C E = True
k D E = True
k E E = True

k A D = True
k B D = True
k C D = True
k D D = True
k E D = True

k A A = True
k B A = False
k C A = True
k D A = False
k E A = False

k A B = True
k B B = True
k C B = False
k D B = False
k E B = False

k A C = False
k B C = False
k C C = False
k D C = False
k E C = False

-- nub function
nub                     :: (Eq a) => [a] -> [a]
nub                     =  nubBy (==)
nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)

--Aufgabe 2: Strom ---
--stream :: [Integer]
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib ::	 Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
