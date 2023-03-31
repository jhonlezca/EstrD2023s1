-- Recursion sobre listas 
--1. 

--Dada una lista de enteros devuelve la suma de todos sus elementos.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

{-2. 
Dada una lista 
elementos de algún tipo devuelve el largo de esa lista, es decir, la cantidad
de elementos que posee. -}

longitud :: [a] -> Int
longitud [] = 0
longitud (n:ns) = 1 + longitud ns


{-
3. 
Dada una lista de enteros, devuelve la lista de los sucesores de cada entero.-}
sucesores :: [Int] -> [Int]
sucesores  [] = []
sucesores (n:ns) = (n+1) : sucesores ns 

--4
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (n:ns) = n && conjuncion ns
{-Dada una lista de booleanos devuelve True si todos sus elementos son True.-}

--5.
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (n:ns) = n || disyuncion ns
{- Dada una lista de booleanos devuelve True si alguno de sus elementos es True.-}

--6
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (ns:nss) = ns ++ aplanar nss


{- Dada una lista de listas, devuelve una única lista con todos sus elementos.-}

--7


pertenece :: Eq a => a -> [a] -> Bool
pertenece  _ [] = False
pertenece e (n:ns) = e == n || pertenece e ns

 
--8
apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones e (x:xs) = iguales e x + (apariciones e xs) 

-- iguales: dado 2 elementos devuelve 1 si son iguales
-- 0 en caso contrario 

iguales :: Eq a => a -> a -> Int
iguales x y = if x == y
                then 1
                else 0 


--9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []= []
losMenoresA n (x:xs)  = if(n > x)
then x : losMenoresA n xs
else losMenoresA n xs



--10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] =[]
lasDeLongitudMayorA n (xs:xss) =
 if n < longitud xs 
 then xs: lasDeLongitudMayorA n xss 
 else lasDeLongitudMayorA n xss

--11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] m = [m]
agregarAlFinal (n:ns) m= n: agregarAlFinal ns m

--12
agregar :: [a] -> [a] -> [a]
agregar [] ys     = ys
agregar (x:xs) ys = x : agregar xs ys  

--13
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

--14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] _ = []
zipMaximos _ [] =[]
zipMaximos (x:xs)(y:ys)= if x > y 
then x : zipMaximos xs ys
else y: zipMaximos xs ys


--15

elMinimo :: Ord a => [a] -> a
elMinimo [x] = x
elMinimo (x:xs) = if x < (elMinimo xs)
                    then x
                    else elMinimo xs

  


-- PUNTO 2 ,recursion sobre numeros
--1  
factorial :: Int -> Int
factorial 0 = 1
factorial n =  n * factorial (n-1)
--2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva (-1) = []
cuentaRegresiva n = if n < 0
then []
else n : cuentaRegresiva (n-1)

--3
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e: repetir (n-1) e
--4
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x:xs) =  x: losPrimeros (n-1) xs

--5
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 1 (_:xs) = xs
sinLosPrimeros _ [] = []
sinLosPrimeros n (x:xs)= sinLosPrimeros (n-1) xs


{-
  3. REGISTROS
-}



data Persona = P String Int
 deriving Show 

-- mayorA: dado 1 número y una persona
-- retorna si la edad de la persona es mayor  al número dado

mayorA :: Int -> Persona -> Bool 
mayorA  n (P _ edad) = edad > n

--1
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ [] = []
mayoresA n (x:xs)= if mayorA n x
then x : mayoresA n xs
else mayoresA n xs


edad :: Persona -> Int 
edad (P n e) = e
sumatoriaEdad:: [Persona]-> Int


sumatoriaEdad []= 0
sumatoriaEdad (x:xs)= edad x + sumatoriaEdad xs



--3
promedioEdad :: [Persona] -> Int
promedioEdad x = div (sumatoriaEdad x) (longitud x)
{-
Dada una lista de personas devuelve el promedio de edad entre esas personas. Precon-dición: la lista al menos posee una persona.
-}


--4

elMasViejo :: [Persona] -> Persona
elMasViejo [] = error "no hay nadieeee"
elMasViejo [x]     = x
elMasViejo (x:xs) = if edad x > edad(elMasViejo xs)
then x
else elMasViejo xs



-- POKEMON 

data TipoDePokemon = Agua | Fuego | Planta
 deriving Show 
data Pokemon = Pk TipoDePokemon Int
 deriving Show
data Entrenador = E String [Pokemon]
 deriving Show

--1
cantPokemon :: Entrenador -> Int
cantPokemon (E _ xs) = longitud xs

tipoPokemon :: Pokemon-> TipoDePokemon 
tipoPokemon (Pk tipo _)= tipo

{-
mismoTipo: dado 2 tipos de pokemon retorna True si son iguales False en caso contrario
-}
mismoTipo::TipoDePokemon -> TipoDePokemon -> Bool
mismoTipo Planta Planta = True
mismoTipo Fuego Fuego = True 
mismoTipo Agua Agua =True 
mismoTipo _ _ = False 

{-
esDe: dado 1 tipo de pokemon y un pokemon retorna 1 si el pokemon es del mismo tipo dado , 0 en caso contrario
-}
esDe::TipoDePokemon->  Pokemon->Int 
esDe t (Pk tipo _ )= if mismoTipo t tipo
then 1
else 0


--2
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe _ (E _ [])= 0
cantPokemonDe tipo (E n (x:xs))=  (esDe tipo x )+( cantPokemonDe tipo (E n xs ))

--4
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e  = cantPokemonDe Planta e >0 && cantPokemonDe Fuego e >0 && cantPokemonDe Agua e >0



cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ tipo e1 e2= if (cantPokemonDe tipo e1) < (cantPokemonDe (nemesis tipo) e2)
then cantPokemonDe tipo e1
else cantPokemonDe (nemesis tipo) e2

nemesis :: TipoDePokemon -> TipoDePokemon
nemesis Agua = Fuego
nemesis Planta = Agua 
nemesis Fuego = Planta 


lapras = Pk Agua 100
char   = Pk  Fuego 100
chiko  = Pk  Planta 100

joni= P "jhon" 1006
ema =P "ema" 15035
mari = P "mar" 33
maria = P "maria" 500

jhon= E "joni "[chiko,lapras, char, lapras]

emau = E "ema" [char]
