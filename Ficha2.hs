import Data.Char
import Distribution.Simple.Utils (xargs)

-- 2. Defina recursivamente as seguintes funções sobre listas:

-- a) Recebe uma lista e produz a lista em que cada elemento é o dobro do valor correspondente na lista de entrada.
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2*h : dobros t 

-- b) Calcula o número de vezes que um caracter ocorre numa string.
numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre c (h:t)
    | c == h = 1 + numOcorre c t
    | otherwise = numOcorre c t

-- c) Testa se uma lista só tem elementos positivos.
positivos :: [Int] -> Bool
positivos [] = True
positivos (h:t) = h > 0 && positivos t

-- d) Retira todos os elementos não positivos de uma lista de inteiros.
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t)
    | h > 0 = h : soPos t
    | otherwise = soPos t 

-- e) Soma todos os números negativos da lista de entrada.
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t)
    | h < 0 = h + somaNeg t
    | otherwise = somaNeg t

-- f) Devolve os últimos três elementos de uma lista. Se a lista de entrada tiver menos de trẽs elementos, devolve a própria lista.
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (a:b:c:d:t) = tresUlt (b:c:d:t)
tresUlt l = l

-- g) Calcula a lista das segundas componentes dos pares.
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((_,y):t) = y : segundos t

-- h) Testa se um elemento aparece na lista como primeira componente de algum dos pares.
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros a ((x,_):t)
    | a == x = True
    | otherwise = nosPrimeiros a t

-- i) Soma uma lista de triplos componente a componente.
-- exemplo: sumtriplos [(2,4,11), (3,1,-5), (10,-3,6)] = (15,2,12)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((x,y,z):t) = (x + sx,y + sy,z + sz)
    where
        (sx,sy,sz) = sumTriplos t 

-- 3. Recorrendo a funções do módulo Data.Char, defina recursivamente as seguintes funções sobre strings:

-- a) Recebe uma lista de caracteres, e seleciona dessa lista os caracteres que são algarismos.
soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t)
    | isDigit h = h : soDigitos t
    | otherwise = soDigitos t

-- b) Recebe uma lista de caracteres, e conta quantos desses caracteres são letras minúsculas.
minusculas :: [Char] -> Int
minusculas [] = 0
minusculas (h:t)
    | isLower h = 1 + minusculas t
    | otherwise = minusculas t

-- c) Recebe uma string e devolve uma lista com os algarismos que ocorrem nessa string, pela mesma ordem.
nums :: String -> [Int]
nums [] = []
nums (h:t)
    | isDigit h = digitToInt h : nums t
    | otherwise = nums t

-- 4. Uma forma de representar polinómios de uma variável é usar listas de monómios representados por pares (coeficiente, expoente)

type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- Por exemplo, [(2,3), (3,4), (5,3), (4,5)] representa o polinómio 2x^3 + 3x^4 + 5x^3 + 4x^5.

-- a) (conta n p) indica quantos monómios de grau n existem em p.
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((_,e):t)
    | n == e = 1 + conta n t
    | otherwise = conta n t

-- b) Indica o grau de um polinómio.
grau :: Polinomio -> Int
grau [] = 0
grau ((_,e):t) = max e (grau t)

-- c) Seleciona os monómios com um dado grau de um monómio.
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((c,e):t)
    | n == e = (c,e) : selgrau n t
    | otherwise = selgrau n t

-- d) Calcula a derivada de um polinómio.
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c,e):t)
    | e > 0 = (c * fromIntegral e, e - 1) : deriv t
    | otherwise = deriv t

-- e) Calcula o valor de um polinómio para um dado valor de x.
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula x ((c,e):t) = c * (x^e) + calcula x t

-- f) Retira de um polinómio os monómios de coeficiente zero.
simp :: Polinomio -> Polinomio
simp [] = []
simp ((c,e): t)
    | c /= 0 = (c,e) : simp t
    | otherwise = simp t

-- g) Calcula o resultado da multiplicação de um monómio por um polinómio.
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (cm,em) ((c,e):t) = (cm * c,em + e) : mult (cm,em) t

-- h) Dado um polinómio constrói um polinómio equivalente em que não podem aparecer vários monómios com o mesmo grau
insere :: Monomio -> Polinomio -> Polinomio
insere (cm,em) [] = [(cm,em)]
insere (cm,em) ((c,e):t)
    | em == e = (cm + c,e) : t
    | otherwise = (c,e) : insere (cm,em) t

normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza (m:t) = insere m (normaliza t)

-- i) Soma dois polinómios de forma a que se os polinómios que recebe estiverem normalizados produz também um polinómio normalizado.
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

-- j) Calcula o produto de dois polinómios
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto (m:t) p2 = soma (mult m p2) (produto t p2)

-- k) Ordena um polinómio por ordem cresecente dos graus dos seus monómios.
insereOrd :: Monomio -> Polinomio -> Polinomio
insereOrd m [] = [m]
insereOrd (cm,em) ((c,e):t)
    | em <= e = (cm,em) : (c,e) : t 
    | otherwise = (c,e) : insereOrd (cm,em) t

ordena :: Polinomio -> Polinomio
ordena [] = []
ordena (m:t) = insereOrd m (ordena t)

-- l) Testa se dois polinómios são equivalentes.
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = transforma p1 == transforma p2
    where
        transforma p = ordena (simp (normaliza p))