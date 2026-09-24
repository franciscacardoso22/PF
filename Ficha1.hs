import Data.Char

--Exercício 1
perimetro :: Double -> Double
perimetro r = 2*pi*r

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

primUlt :: [a] -> (a,a)
primUlt l = (head l,last l)

multiplo :: Int -> Int -> Bool
multiplo m n 
    | mod m n == 0 = True
    | otherwise = False

truncaImpar :: [a] -> [a]
truncaImpar l 
    | mod (length l) 2 /= 0 = tail l
    | otherwise = l 

max2 :: Int -> Int -> Int
max2 x y
    | x >= y = x
    | otherwise = y

max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 x (max2 y z)

--Exercício 2
nRaizes :: Double -> Double -> Double -> Int
nRaizes x y z
    | delta > 0 = 2
    | delta == 0 = 1
    | otherwise = 0
    where delta = y^2 - 4*x*z

raizes :: Double -> Double -> Double -> [Double]
raizes x y z
    | delta < 0 = []
    | delta == 0 = [(-y)/(2*x)]
    | otherwise = [(-y + sqrt delta)/(2*x), (-y - sqrt delta)/(2*x)]
    where delta = y^2 - 4*x*z

--Exercício 3
type Hora = (Int,Int)

horaVal :: (Int,Int) -> Bool
horaVal (h,m)
    | h >= 0 && h < 24 && m >= 0 && m < 60 = True
    | otherwise = False

horaDepois :: Hora -> Hora -> Bool
horaDepois (h1,m1) (h2,m2)
    | h1 > h2 = True
    | h1 == h2 && m1 > m2 = True
    | otherwise = False

horasMin :: Hora -> Int
horasMin (h,m) = h*60 + m

minHoras :: Int -> Hora
minHoras m = (div m 60,mod m 60)

difHoras :: Hora -> Hora -> Int
difHoras h1 h2 = abs(horasMin h1 - horasMin h2)

maisMin :: Hora -> Int -> Hora
maisMin (h,m) x = minHoras(horasMin (h,m) + x)

--Exercício 4
data Hora' = H Int Int deriving (Show,Eq)

horaVal' :: Hora' -> Bool
horaVal' (H h m)
    | h >= 0 && 24 > h && m >= 0 && 60 > m = True
    | otherwise = False

horaDepois' :: Hora' -> Hora' -> Bool
horaDepois' (H h1 m1) (H h2 m2)
    | h1 > h2 = True
    | h1 == h2 && m1 > m2 = True
    | otherwise = False

horasMin' :: Hora' -> Int
horasMin' (H h m) = 60*h + m

minHoras' :: Int -> Hora'
minHoras' m = (H (div m 60) (mod m 60))

difHoras' :: Hora' -> Hora' -> Int
difHoras' (H h1 m1) (H h2 m2) = abs(horasMin'(H h1 m1) - horasMin'(H h2 m2))

maisMin' :: Hora' -> Int -> Hora'
maisMin' (H h m) x = minHoras'(horasMin'(H h m) + x)

--Exercício 5
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

safe :: Semaforo -> Semaforo -> Bool
safe _ Vermelho = True
safe Vermelho _ = True 
safe _ _ = False

--Exercício 6
data Ponto = Cartesiano Double Double | Polar Double Double
            deriving (Show,Eq)

posx :: Ponto -> Double
posx (Cartesiano x _) = x
posx (Polar r a) = abs (r * cos a)

posy :: Ponto -> Double
posy (Cartesiano _ y) = abs y
posy (Polar r a) = abs (r * sin a)

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar r _) = r

angulo :: Ponto -> Double
angulo (Cartesiano x y)
    | x == 0 && y == 0 = 0
    | x == 0 && y > 0 = pi/2
    | x == 0 && y < 0 = -(pi/2)
    | x > 0 = atan(y/x)
    | x < 0 && y >= 0 = atan (y/x) + pi
    | x < 0 && y < 0 = atan (y/x) - pi
angulo (Polar _ a) = a 

-- Função auxiliar: normaliza qualquer Ponto para o par cartesiano (x,y)
pontoParaCart :: Ponto -> (Double,Double)
pontoParaCart (Cartesiano x y) = (x,y)
pontoParaCart (Polar r a) = (r * cos a,r * sin a)

-- Função principal
dist' :: Ponto -> Ponto -> Double
dist' p1 p2 = sqrt((x1 - x2)^2 + (y1 - y2)^2)
    where 
        (x1,y1) = pontoParaCart p1 
        (x2,y2) = pontoParaCart p2

-- Exercício 7

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono _ = True

vertices :: Figura -> [Ponto]
vertices (Circulo _ _ ) = []
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]
vertices (Retangulo p1 p2) = [p1, Cartesiano x1 y2, p2, Cartesiano x2 y1]
    where
        (x1,y1) = pontoParaCart p1
        (x2,y2) = pontoParaCart p2

area :: Figura -> Double 
area (Triangulo p1 p2 p3) =
    let a = dist' p1 p2 
        b = dist' p2 p3
        c = dist' p3 p1 
        s = (a+b+c)/2 --semi-perimetro
    in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Retangulo p1 p2) = abs(x1 - x2) * abs (y1 - y2)
    where 
        (x1,y1) = pontoParaCart p1
        (x2,y2) = pontoParaCart p2
area (Circulo _ r) = pi * r^2 

perimetro' :: Figura -> Double
perimetro' (Circulo _ r) = 2*pi*r
perimetro' (Triangulo p1 p2 p3) = dist' p1 p2 + dist' p2 p3 + dist' p3 p1
perimetro' (Retangulo p1 p2) = 2*abs(x1-x2) + 2*abs(y1-y2)
    where
        (x1,y1) = pontoParaCart p1 
        (x2,y2) = pontoParaCart p2

-- Exercício 8

-- ord :: Char -> Int (faz o mesmo que fromEnum)
-- devolve o código numérico do caráter (ex: ord 'A' é 65)

-- chr :: Int -> Char (faz o mesmo que toEnum)
-- faz o caminho inverso, converte o código numérico no caráter (ex: chr 97 devolve 'a')

isLower' :: Char -> Bool
isLower' c = c >= 'a' && c <= 'z'

isDigit' :: Char -> Bool
isDigit' c = c >= '0' && c <= '9'

isUpper' :: Char -> Bool
isUpper' c = c >= 'A' && c <= 'Z'

isAlpha' :: Char -> Bool
isAlpha' c = isLower' c || isUpper' c 

toUpper' :: Char -> Char
toUpper' c
    | isLower' c = chr (ord c - (ord 'a' - ord 'A'))
    | otherwise = c

intToDigit' :: Int -> Char
intToDigit' n
    | n >= 0 && n <= 9 = chr (ord '0' + n)

digitToInt' :: Char -> Int
digitToInt' c
    | isDigit' c = ord c - ord '0'
