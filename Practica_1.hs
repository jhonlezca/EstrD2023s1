
--Numeos enteros
-- a)

sucesor :: Int -> Int
sucesor n = n+1

--b)

sumar :: Int -> Int -> Int
sumar n m = n+m

--c)

divisionYResto :: Int -> Int -> ( Int, Int)
divisionYResto n m = ( div n m, mod n m)

--d)
maxDelPar :: (Int, Int) -> Int
maxDelPar (n, m) = if n > m 
                    then n
                    else m

--ejemplo 1--
-- sumaar 4 6
--ejemplo 2--
-- sucesor 9
-- ejemplo 3 --
-- sucesor ( maxDelPar (9,9) )
-- ejemplo 4 --
--  maxDelPar (divisionYResto 100 10)

-- Tipos enumerativos

-- 3_1
-- a)

data Dir = Norte| Este | Sur| Oeste
    deriving Show

opuesto :: Dir -> Dir
opuesto d = 
    case d of
        Norte -> Sur
        Sur   -> Norte
        Este  -> Oeste
        Oeste -> Este

--b)

iguales :: Dir -> Dir -> Bool
iguales  Norte Norte = True
iguales  Sur  Sur    = True
iguales  Este Este   = True
iguales  Oeste Oeste = True
iguales  _ _         = False

-- c)
siguiente :: Dir -> Dir
siguiente d = 
    case d of 
        Norte -> Este
        Sur   -> Oeste
        Este  -> Sur
        Oeste -> Norte


--3_2

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo 

--a)
primeroYUltimoDia:: (DiaDeSemana,DiaDeSemana)
primeroYUltimoDia = (Lunes,Domingo)

{-
  El enunciado dice que la función devuelve el par (Lunes,Domingo) nose si entendí mal pero ademas pedía hacer subtareas para que resulte mas fácil el ejercicio pero lo hice en 2 lineas la verdad o entendí mal o hay un error de tipeo
-}



--b)
empiezaConM :: DiaDeSemana -> Bool

empiezaConM Martes    = True
empiezaConM Miercoles = True
empiezaConM  _        = False 

--c)
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool

vieneDespues Lunes Martes     = True 
vieneDespues Martes miercoles = True
vieneDespues Miercoles Jueves = True
vieneDespues Jueves Viernes   = True
vieneDespues Viernes Sabado   = True
vieneDespues Sabado Domingo   = True
vieneDespues Domingo Lunes    = True
vieneDespues _ _              = False

--d)

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Domingo  = False 
estaEnElMedio Lunes    = False 
estaEnElMedio _        = True 


--3_3
--a)
negar:: Bool -> Bool 
negar True = False 
negar False = True 


--b) 

implica :: Bool -> Bool -> Bool
implica  True False = False
implica  _ _        = True

--c)

yTambien :: Bool -> Bool -> Bool
yTambien True True = True 
yTambien _ _ = False 

--d )

oBien :: Bool -> Bool -> Bool
oBien False False = False 
oBien _ _ = True

--4. Registros

--4_1

data Persona = P String Int
 deriving Show

nombre :: Persona -> String
nombre (P n e)  = n

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer (P n e)  = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nuevoNombre (P n e) = P nuevoNombre e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra p p2 = edad p > edad p2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if( esMayorQueLaOtra p1 p2)
                     then p1
                     else p2 

joni = P "joni" 32
david = P "david" 4


--4_2

data TipoDePokemon = Agua | Fuego | Planta
 deriving Show
data Pokemon = PK String TipoDePokemon Int
 deriving Show
data Entrenador = E String Pokemon Pokemon
 deriving Show

tipoPokemon :: Pokemon -> TipoDePokemon
tipoPokemon (PK _ t e) =  t

tablaDeTipos :: TipoDePokemon -> TipoDePokemon -> Bool
tablaDeTipos Agua Fuego   = True
tablaDeTipos Fuego Planta = True 
tablaDeTipos Planta Agua  = True
tablaDeTipos _ _          = False


superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = tablaDeTipos (tipoPokemon p1) (tipoPokemon p2)


charmander = PK "charmander" Fuego 100
odish  = PK "odish" Planta 100
lapras  = PK "lapras" Agua 100

jhon = E "jhon"  odish odish
gary = E "gary"  charmander lapras
bruck = E "bruck" charmander charmander
mysti = E "mysti"  lapras lapras

contarTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Int
contarTipoDePokemon Agua Agua = 1
contarTipoDePokemon Fuego Fuego = 1
contarTipoDePokemon Planta Planta = 1 
contarTipoDePokemon _ _ = 0

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe  tipo (E n pk1 pk2) = contarTipoDePokemon tipo (tipoPokemon pk2) + contarTipoDePokemon tipo (tipoPokemon pk1)

listarPokemones :: Entrenador -> [Pokemon]
listarPokemones (E n pk1 pk2)  = [pk1, pk2]

juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon ( E n pk1 pk2 , E m pk3 pk4) = listarPokemones (E n pk1 pk2)++ listarPokemones (E m pk3 pk4)

--5
loMismo :: a -> a
loMismo a = a

--b) 
siempreSiete :: a -> Int
siempreSiete a = 7

--c) 
swap :: (a,b) -> (b, a)
swap (a,b)= (b,a)

--6

estaVacia :: [a] -> Bool
estaVacia  [] = True
estaVacia  (_:_) = False 
 
--elPrimero :: [a] -> a
elPrimero (x:) = x
elPrimero _ = error " no hay primer elemento"

--4

sinElPrimero :: [a] -> [a]
sinElPrimero a = tail a

--5

splitHead :: [a] -> (a, [a])
splitHead a = ( elPrimero a , sinElPrimero a)   