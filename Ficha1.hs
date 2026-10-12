import Data.Char

-- 1. Usando as seguintes funções pré-definidas do Haskell:
--  • length l: o número de elementos da lista l
--  • head l: a cabeça da lista (não vazia) l
--  • tail l: a cauda da lista (não vazia) l
--  • last l: o último elemento da lista (não vazia) l
--  • sqrt x: a raiz quadrada de x
--  • div x y: a divisão inteira de x por y
--  • mod x y: o resto da divisão inteira de x por y
-- Defina as seguintes funções e os respectivos tipos:

-- a) perimetro – que calcula o perı́metro de uma circunferência, dado o comprimento do seu raio.
perimetro :: Double -> Double
perimetro r = 2*pi*r


-- b) dist – que calcula a distância entre dois pontos no plano Cartesiano. Cada ponto é um par de valores do tipo Double.
dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt((x2-x1)^2 + (y2-y1)^2)

-- c) primUlt – que recebe uma lista e devolve um par com o primeiro e o último elemento dessa lista.
primUlt :: [a] -> (a,a)
primUlt l = (head l,last l)

-- d) multiplo – tal que multiplo m n testa se o número inteiro m é múltiplo de n.
multiplo :: Int -> Int -> Bool
multiplo m n 
    | mod m n == 0 = True
    | otherwise = False

-- e) truncaImpar – que recebe uma lista e, se o comprimento da lista for ı́mpar retira-lhe o primeiro elemento, caso contrário devolve a própria lista.
truncaImpar :: [a] -> [a]
truncaImpar l 
    | mod (length l) 2 /= 0 = tail l
    | otherwise = l 

-- f) max2 – que calcula o maior de dois números inteiros.
max2 :: Int -> Int -> Int
max2 x y
    | x >= y = x
    | otherwise = y

-- g) max3 – que calcula o maior de três números inteiros, usando a função max2.
max3 :: Int -> Int -> Int -> Int
max3 x y z = max2 x (max2 y z)


-- 2. Defina as seguintes funções sobre polinómios de 2º grau:

-- a) A função nRaizes que recebe os (3) coeficientes de um polinómio de 2o grau e que calcula o número de raı́zes (reais) desse polinómio.
nRaizes :: Double -> Double -> Double -> Int
nRaizes x y z
    | delta > 0 = 2
    | delta == 0 = 1
    | otherwise = 0
    where delta = y^2 - 4*x*z

-- b) A função raizes que, usando a função anterior, recebe os coeficientes do polinómio e calcula a lista das suas raı́zes reais.
raizes :: Double -> Double -> Double -> [Double]
raizes x y z
    | delta < 0 = []
    | delta == 0 = [(-y)/(2*x)]
    | otherwise = [(-y + sqrt delta)/(2*x), (-y - sqrt delta)/(2*x)]
    where delta = y^2 - 4*x*z

-- 3. Vamos representar horas por um par de números inteiros:
-- type Hora = (Int,Int)
-- Assim o par (0,15) significa meia noite e um quarto e (13,45) duas menos um quarto.
-- Defina funções para:

type Hora = (Int,Int)

-- a) Testar se um par de inteiros representa uma hora do dia válida.
horaVal :: (Int,Int) -> Bool
horaVal (h,m)
    | h >= 0 && h < 24 && m >= 0 && m < 60 = True
    | otherwise = False

-- b) Testar se uma hora é ou não depois de outra (comparação).
horaDepois :: Hora -> Hora -> Bool
horaDepois (h1,m1) (h2,m2)
    | h1 > h2 = True
    | h1 == h2 && m1 > m2 = True
    | otherwise = False

-- c) Converter um valor em horas (par de inteiros) para minutos (inteiro).
horasMin :: Hora -> Int
horasMin (h,m) = h*60 + m


-- d) Converter um valor em minutos para horas.
minHoras :: Int -> Hora
minHoras m = (div m 60,mod m 60)

-- e) Calcular a diferença entre duas horas (cujo resultado deve ser o número de minutos).
difHoras :: Hora -> Hora -> Int
difHoras h1 h2 = abs(horasMin h1 - horasMin h2)

-- f) Adicionar um determinado número de minutos a uma dada hora.
maisMin :: Hora -> Int -> Hora
maisMin (h,m) x = minHoras(horasMin (h,m) + x)

-- 4. Repita o exercı́cio anterior assumindo agora que as horas são representadas por um novo tipo de dados:
data Hora' = H Int Int deriving (Show,Eq)

-- Com este novo tipo a hora meia noite e um quarto é representada por H 0 15 e a hora duas menos um quarto por H 13 45.

-- a)
horaVal' :: Hora' -> Bool
horaVal' (H h m)
    | h >= 0 && 24 > h && m >= 0 && 60 > m = True
    | otherwise = False

-- b)
horaDepois' :: Hora' -> Hora' -> Bool
horaDepois' (H h1 m1) (H h2 m2)
    | h1 > h2 = True
    | h1 == h2 && m1 > m2 = True
    | otherwise = False

-- c)
horasMin' :: Hora' -> Int
horasMin' (H h m) = 60*h + m

-- d)
minHoras' :: Int -> Hora'
minHoras' m = (H (div m 60) (mod m 60))

-- e)
difHoras' :: Hora' -> Hora' -> Int
difHoras' (H h1 m1) (H h2 m2) = abs(horasMin'(H h1 m1) - horasMin'(H h2 m2))

-- f)
maisMin' :: Hora' -> Int -> Hora'
maisMin' (H h m) x = minHoras'(horasMin'(H h m) + x)

-- 5.Considere o seguinte tipo de dados para representar os possı́veis estados de um semáforo:
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

-- a) Defina a função next :: Semaforo -> Semaforo que calcula o próximo estado de um semáforo.
next :: Semaforo -> Semaforo
next Verde = Amarelo
next Amarelo = Vermelho
next Vermelho = Verde

-- b) Defina a função stop :: Semaforo -> Bool que determina se é obrigatório parar num semáforo.
stop :: Semaforo -> Bool
stop Vermelho = True
stop _ = False

-- c) Defina a função safe :: Semaforo -> Semaforo -> Bool que testa se o estado de dois semáforos num cruzamento é seguro.
safe :: Semaforo -> Semaforo -> Bool
safe _ Vermelho = True
safe Vermelho _ = True 
safe _ _ = False

-- 6. Um ponto num plano pode ser representado por um sistema de coordenadas Cartesiano (distâncias aos eixos vertical e horizontal) ou por um sistema de coordenadas Polar (distância à origem e ângulo do respectivo vector com o eixo horizontal).
data Ponto = Cartesiano Double Double | Polar Double Double
            deriving (Show,Eq)

-- Com este tipo o ponto Cartesiano (-1) 0 pode alternativamente ser representado por Polar 1 pi. Defina as seguintes funções:

-- a) posx :: Ponto -> Double que calcula a distância de um ponto ao eixo vertical.
posx :: Ponto -> Double
posx (Cartesiano x _) = x
posx (Polar r a) = abs (r * cos a)

-- b) posy :: Ponto -> Double que calcula a distância de um ponto ao eixo horizontal.
posy :: Ponto -> Double
posy (Cartesiano _ y) = abs y
posy (Polar r a) = abs (r * sin a)

-- c) raio :: Ponto -> Double que calcula a distância de um ponto à origem.
raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar r _) = r

-- d) angulo :: Ponto -> Double que calcula o ângulo entre o vector que liga a origem ao ponto e o eixo horizontal.
angulo :: Ponto -> Double
angulo (Cartesiano x y)
    | x == 0 && y == 0 = 0
    | x == 0 && y > 0 = pi/2
    | x == 0 && y < 0 = -(pi/2)
    | x > 0 = atan(y/x)
    | x < 0 && y >= 0 = atan (y/x) + pi
    | x < 0 && y < 0 = atan (y/x) - pi
angulo (Polar _ a) = a 


-- dist :: Ponto -> Ponto -> Double que calcula a distância entre dois pontos.
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

-- 7. Considere o seguinte tipo de dados para representar figuras geométricas num plano.

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)
-- O tipo de dados diz que uma figura pode ser um cı́rculo centrado num determinado ponto e com um determinado raio, um rectângulo paralelo aos eixos representado por dois pontos que são vértices da sua diagonal, ou um triângulo representado pelos três pontos dos seus vértices.
-- Defina as seguintes funções:

-- a) Defina a função poligono :: Figura -> Bool que testa se uma figura é um polı́gono.
poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono _ = True

-- b) Defina a função vertices :: Figura -> [Ponto] que calcula a lista dos vértices de uma figura.
vertices :: Figura -> [Ponto]
vertices (Circulo _ _ ) = []
vertices (Triangulo p1 p2 p3) = [p1,p2,p3]
vertices (Retangulo p1 p2) = [p1, Cartesiano x1 y2, p2, Cartesiano x2 y1]
    where
        (x1,y1) = pontoParaCart p1
        (x2,y2) = pontoParaCart p2

-- c) Calcular a área de uma figura:
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

-- d) Defina a função perimetro :: Figura -> Double que calcula o perı́metro de uma figura.
perimetro' :: Figura -> Double
perimetro' (Circulo _ r) = 2*pi*r
perimetro' (Triangulo p1 p2 p3) = dist' p1 p2 + dist' p2 p3 + dist' p3 p1
perimetro' (Retangulo p1 p2) = 2*abs(x1-x2) + 2*abs(y1-y2)
    where
        (x1,y1) = pontoParaCart p1 
        (x2,y2) = pontoParaCart p2

-- 8. Utilizando as funções ord :: Char -> Int e chr :: Int -> Char do módulo Data.Char, defina as seguintes funções:

-- ord :: Char -> Int (faz o mesmo que fromEnum)
-- devolve o código numérico do caráter (ex: ord 'A' é 65)

-- chr :: Int -> Char (faz o mesmo que toEnum)
-- faz o caminho inverso, converte o código numérico no caráter (ex: chr 97 devolve 'a')

-- a) isLower :: Char -> Bool, que testa se um Char é uma minúscula.
isLower' :: Char -> Bool
isLower' c = c >= 'a' && c <= 'z'

-- b) isDigit :: Char -> Bool, que testa se um Char é um dı́gito.
isDigit' :: Char -> Bool
isDigit' c = c >= '0' && c <= '9'

-- c) isAlpha :: Char -> Bool, que testa se um Char é uma letra.
isAlpha' :: Char -> Bool
isAlpha' c = isLower' c || isUpper' c 

isUpper' :: Char -> Bool
isUpper' c = c >= 'A' && c <= 'Z'

-- d) toUpper :: Char -> Char, que converte uma letra para a respectiva maiúscula.
toUpper' :: Char -> Char
toUpper' c
    | isLower' c = chr (ord c - (ord 'a' - ord 'A'))
    | otherwise = c

-- e) intToDigit :: Int -> Char, que converte um número entre 0 e 9 para o respetivo dı́gito.
intToDigit' :: Int -> Char
intToDigit' n
    | n >= 0 && n <= 9 = chr (ord '0' + n)

-- f) digitToInt :: Char -> Int, que converte um dı́gito para o respectivo inteiro.
digitToInt' :: Char -> Int
digitToInt' c
    | isDigit' c = ord c - ord '0'
