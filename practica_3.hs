{-
1. Tipos recursivos simples
1.1. Celdas con bolitas
-}

data Color = Azul | Rojo 
  deriving Show 
data Celda = Bolita Color Celda | CeldaVacia
  deriving Show 





nroBolitas :: Color -> Celda -> Int
nroBolitas  col CeldaVacia = 0
nroBolitas col (Bolita c cel) = unoSi (mismoColor col c)+ (nroBolitas col cel )

mismoColor :: Color-> Color -> Bool 
mismoColor  Azul Azul = True 
mismoColor  Rojo Rojo = True 
mismoColor  _  _  = False

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0




poner :: Color -> Celda -> Celda
poner col cel = Bolita col cel


sacar :: Color -> Celda -> Celda
sacar col CeldaVacia = CeldaVacia 
sacar col (Bolita c celda ) = if mismoColor col c
then celda
else (Bolita c (sacar col celda))


ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 col cel = cel
ponerN n col cel = ponerN (n-1 ) col (poner col cel)

{-
1.2. Camino hacia el tesoro
Tenemos los siguientes tipos de datos
-}
data Objeto = Cacharro | Tesoro
 deriving Show 

data Camino = Fin |Cofre [Objeto] Camino | Nada Camino
 deriving Show 

monedas= Tesoro 
basura = Cacharro 

cam0= Fin 
cam1 = Cofre[monedas] cam0
cam2 = Cofre[basura] cam1
cam3 =  Cofre [monedas] cam1
cam4= Cofre[monedas] cam3
cam5 = Nada (Cofre[basura] (Cofre[monedas,monedas] (Nada (Nada (Nada (Cofre[monedas] (Cofre [monedas] Fin)))))))




hayTesoro :: Camino -> Bool
hayTesoro Fin = False
hayTesoro (Nada c) = hayTesoro c
hayTesoro (Cofre xs c) = cofreConTesoro xs || hayTesoro c

cofreConTesoro:: [Objeto] -> Bool
cofreConTesoro [] = False
cofreConTesoro (x:xs) = esTesoro x || cofreConTesoro xs 

esTesoro:: Objeto -> Bool
esTesoro Tesoro = True 
esTesoro _      = False 




pasosHastaTesoro:: Camino ->  Int 
pasosHastaTesoro Fin = error"no cumple con la precondicion : Debe haber por lo menos 1 tesoro en el camino"
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c
pasosHastaTesoro (Cofre xs c)= if cofreConTesoro xs
then 0
else 1 + pasosHastaTesoro c



hayTesoroEn:: Int -> Camino -> Bool
hayTesoroEn  0 c  = halleTesoro c
hayTesoroEn  n c  = hayTesoroEn (n-1) (siguienteCamino c)

-- halleTesoro: determina si en el camino actual hay un tesoro
halleTesoro:: Camino -> Bool
halleTesoro (Cofre xs c) = cofreConTesoro xs
halleTesoro     _        = False

siguienteCamino :: Camino -> Camino 
siguienteCamino Fin = Fin
siguienteCamino (Nada c ) = c
siguienteCamino (Cofre xs c) = c


alMenosNTesoros::Int->Camino-> Bool
alMenosNTesoros _ Fin = False
alMenosNTesoros 0 _   = True 
alMenosNTesoros n (Nada c)= alMenosNTesoros n c
alMenosNTesoros n c   = n <= contarTesoros c || alMenosNTesoros (n -contarTesoros c) (siguienteCamino c)



cantDeTesorosEn :: Camino -> Int 
cantDeTesorosEn Fin = 0
cantDeTesorosEn (Nada c) = cantDeTesorosEn c
cantDeTesorosEn (Cofre xs c) = unoSi(cofreConTesoro xs) + cantDeTesorosEn c


cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre  desde hasta camino = (contarTesorosHasta hasta camino) - (contarTesorosHasta (desde-1) camino) 


contarTesorosHasta:: Int -> Camino-> Int
contarTesorosHasta (-1) _   = 0
contarTesorosHasta _ Fin = 0
contarTesorosHasta n c   = (contarTesoros c ) + (contarTesorosHasta (n-1) (siguienteCamino c))

-- contarTesoros dado un camino determina primero si es un cofre de ser asi verifica si dentro del mismo hay tesorosy los cuenta 

contarTesoros:: Camino -> Int
contarTesoros (Cofre xs c) = if cofreConTesoro xs
                                then sumatoriaDeTesoros xs
                                else 0
contarTesoros _ = 0




sumatoriaDeTesoros :: [Objeto] -> Int
sumatoriaDeTesoros []       = 0
sumatoriaDeTesoros ( x:xs ) = unoSi(esTesoro x) + sumatoriaDeTesoros xs


-- Arboles

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
 deriving Show 


nod0 = NodeT "A" (nod1)(nod2)
nod1 = NodeT "B" (nod3) (nod4)
nod2 = NodeT "C" (nod5)(nod6)
nod3 = NodeT "D" EmptyT EmptyT
nod4 = NodeT "E" EmptyT EmptyT
nod5 = NodeT "F" EmptyT EmptyT
nod6 = NodeT "G" EmptyT EmptyT


sumarT :: Tree Int -> Int
sumarT EmptyT = 0
sumarT (NodeT n t1 t2) = n +(sumarT t1) + (sumarT t2)


sizeT :: Tree a -> Int 
sizeT EmptyT = 0
sizeT (NodeT e t1 t2) = 1 + sizeT t1 + sizeT t2

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT n t1 t2) = (NodeT (n*2) (mapDobleT t1) (mapDobleT t2))


perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT = False 
perteneceT e (NodeT i t1 t2) = e==i || perteneceT e t1 || perteneceT e t2

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT = 0
aparicionesT e (NodeT i t1 t2) = unoSi (e==i) + aparicionesT e t1 + aparicionesT e t2

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves  (NodeT i t1 t2) = if esEmptyT t1 && esEmptyT t2
then [i]
else (leaves t1) ++ (leaves t2)

esEmptyT:: Tree a -> Bool 
esEmptyT EmptyT = True
esEmptyT (NodeT a t1 t2) = False


heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT n t1 t2) = 1 + ( mayor  (heightT t1) ( heightT t2 ) )

mayor :: Int -> Int -> Int
mayor a b = if a > b then a else b  

mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT n t1 t2) = (NodeT n (mirrorT t2) (mirrorT t1) ) 

toList :: Tree a -> [a]
toList EmptyT = []
toList (NodeT n t1 t2) = (toList t1) ++ [n] ++ (toList t2)

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN n (NodeT i t1 t2) = if n < 1  then [i] else (levelN (n-1) t1) ++ (levelN (n-1) t2)  



listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT i t1 t2) = [i] : juntarNiveles (listPerLevel t1) (listPerLevel t2)


juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles  []   yss         = yss
juntarNiveles xss  []           = xss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT i t1 t2) = if heightT t1 > heightT t2 then  [i] ++ ramaMasLarga t1 else [i] ++ ramaMasLarga t2

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT i t1 t2) = [i] : consACada i (todosLosCaminos t1) ++ consACada i (todosLosCaminos t2)



consACada :: a -> [[a]] -> [[a]]
consACada x [] = []
consACada x (xs:xss) = (x:xs) : consACada x xss



--2.2 Expresiones

data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA 
  deriving Show

cero = Valor 0
uno = Valor 1
dos = Valor 2
tres= Valor 3
cuatro = Valor 4

suma0 = Sum cero uno 
suma1 = Sum dos cero
suma = Sum cuatro (Sum dos tres)
suma2= Sum suma (Sum suma1 suma0)
suma3 = Sum uno dos

mul  = Prod tres (Sum cuatro (Prod cero uno))


eval :: ExpA -> Int
eval (Valor n)    = n 
eval (Sum v1 v2)  = (eval v1) + (eval v2)
eval (Prod v1 v2) = (eval v1) * (eval v2)
eval (Neg v )     = -(eval v)
