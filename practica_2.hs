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
conjuncion [] = False
conjuncion [x] = x
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
losMenoresA n (x:xs)  = if n > x
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
cuentaRegresiva n = if n < 0
then []
else n : cuentaRegresiva (n-1)

--3
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir n e = e: repetir (n-1) e
--4
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 xs = xs
losPrimeros _ [] = []
losPrimeros n (x:xs) =  x: losPrimeros (n-1) xs

--5
sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ [] = []
sinLosPrimeros 0 xs = xs
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

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0


cantPokemonDe::TipoDePokemon -> Entrenador -> Int 
cantPokemonDe tipo (E apodo xs)= pokemonesDe tipo xs

pokemonesDe:: TipoDePokemon-> [Pokemon]-> Int 
pokemonesDe _ [] = 0
pokemonesDe tipo (x:xs) = unoSi (mismoTipo tipo (tipoPokemon x)) + pokemonesDe tipo xs

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



data Seniority = Junior | SemiSenior | Senior
 deriving Show 
data Proyecto = ConsProyecto String
 deriving Show 
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto

 deriving Show 
data Empresa = ConsEmpresa [Rol]
 deriving Show 

programador1 = Junior
programador2= SemiSenior 
programador3= Senior 


proyecto1 = ConsProyecto "pro1"
proyecto2 = ConsProyecto "pro2"
proyecto3 = ConsProyecto "pro3"
proyecto4 = ConsProyecto "pro4"


rol1= Developer programador1 proyecto1
rol2 = Management programador2 proyecto2
rol3= Developer programador1 proyecto3
rol4 = Management programador3 proyecto4


rol5 = Management programador1 proyecto2
rol6 = Management programador2 proyecto2
rol7 = Management programador3 proyecto2
rol8 = Developer  programador1 proyecto2
rol9 = Developer  programador2 proyecto2



tec= ConsEmpresa [rol2,rol5,rol8]
dat=ConsEmpresa [rol2,rol4,rol2,rol4]
alfa = ConsEmpresa [rol2,rol4,rol2,rol3,rol5]
mec = ConsEmpresa [rol5,rol6,rol7]
{-
Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
-}


proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa xs ) = proyectosSinRepetir xs

proyectosSinRepetir:: [Rol] -> [Proyecto]
proyectosSinRepetir []      = []
proyectosSinRepetir (x:xs)  = if estaRepetido (proyecto x) (proyectosSinRepetir xs)
                                then proyectosSinRepetir xs
                                else proyecto x : proyectosSinRepetir xs

-- estaRepetido dado 1 Proyecto y una Lista de Proyectos  devuelve  True si esta repetido en caso contrario False

estaRepetido :: Proyecto -> [Proyecto] -> Bool
estaRepetido p []     = False
estaRepetido p (x:xs) = mismoProyecto p x || estaRepetido p xs


-- proyecto : dado 1 Rol devuelve el proyecto  del mismo
proyecto :: Rol -> Proyecto 
proyecto (Developer _ p)= p
proyecto (Management _ p)= p


-- mismoProyecto: dado 2 proyectos devuelve True si son iguales en caso contrario  False
mismoProyecto ::Proyecto -> Proyecto -> Bool
mismoProyecto (ConsProyecto s) (ConsProyecto s2)= s==s2


contarDevSenior :: [Rol] -> [Proyecto] -> Int
contarDevSenior [] xs         = 0
contarDevSenior (x:xs) ys = (unoSiEsDevSenior x ys) + (contarDevSenior xs ys )

unoSiEsDevSenior ::Rol -> [Proyecto] -> Int
unoSiEsDevSenior r  ys = if colabaraUnSenior r &&  elProyectoPertenece (proyecto r) ys 
                            then 1
                            else 0


elProyectoPertenece :: Proyecto -> [Proyecto] -> Bool
elProyectoPertenece _  []     =  False 
elProyectoPertenece p ( x:xs) = mismoProyecto p x || elProyectoPertenece p xs 



-- unSenior: dados un Rol y un Proyecto verifica si en dicho Rol colabora un programador senior tambien se verifica si pertenece al proyecto dadocomoparametro


colabaraUnSenior :: Rol -> Bool
colabaraUnSenior (Developer colaborador p) = esSenior colaborador
colabaraUnSenior (Management colaborador p ) = False

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _      = False



--Indica la cantidad de empleados que trabajan en alguno de los proyectos dados


{-
  Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
    
    cantidad de personas involucradas.
-}














