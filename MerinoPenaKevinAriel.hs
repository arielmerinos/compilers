-- Integrantes del equipo:
-- Merino Pena Kevin Ariel | 317031326 | @arielmerinos
-- Aquino Chapa Armando Abraham | 317058163 | @armandoAAC
-- Bonilla Ruiz Roberto Adrian
-- García Toxqui Demian Oswaldo
-- Cristóbal Morales Karen


----------------------------------------------------- EJERCICIO 1 ---------------------------------------------------------------
-- Define la función groupAnagrams tal que recibe una lista de String y devuelve una lista con los anagramas
-- agrupados. Un anagrama es una palabra o frase formada al reorganizar las letras de otra palabra o frase, utilizando
-- todas las letras originales exactamente una vez.

-- | Agrupa una lista de palabras en listas de palabras que son anagramas entre sí.
-- 
-- La función 'groupAnagrams' toma una lista de palabras y devuelve una lista de listas de palabras que son anagramas entre sí. 
-- 
-- >>> groupAnagrams ["roma", "amor", "ramo", "mora", "mar", "armo"]
-- [["roma","amor","ramo","mora"],["mar","armo"]]
-- >>> groupAnagrams ["haskell", "kellash", "lalshke", "lenguaje", "jengual", "jaleguen"]
-- [["haskell","kellash","lalshke"],["lenguaje","jengual","jaleguen"]]
--
groupAnagrams :: [String] -> [[String]]
groupAnagrams = foldr insertAnagram []

-- La función 'insertAnagram' es una función auxiliar que se encarga de insertar una palabra en la lista de anagramas correspondiente.
insertAnagram :: String -> [[String]] -> [[String]]
insertAnagram word [] = [[word]]
insertAnagram word (group:groups)
    | isAnagram word (head group) = (word:group) : groups
    | otherwise = group : insertAnagram word groups

-- La función 'isAnagram' es una función auxiliar que verifica si dos palabras son anagramas entre sí.
isAnagram :: String -> String -> Bool
isAnagram a b = sortString a == sortString b

-- La función 'sortString' es una función auxiliar que ordena los caracteres de una cadena de forma ascendente.
sortString :: String -> String
sortString [] = []
sortString (x:xs) = sortString [y | y <- xs, y <= x]
                    ++ [x] ++
                    sortString [y | y <- xs, y > x]


--    groupAnagrams [ " eat " ," tea " ," tan " ," ate " ," nat " ," bat " ]
--    [[ " eat " ," tea " ," ate " ] ,[ " tan " ," nat " ] ,[ " bat " ]]
--    groupAnagrams [ " hello " ," " ," world " ," wldro " ," hlloe " ," a " ," aa " ]
--    [[ " hello " ," hlloe " ] ,[ " " ] ,[ " world " ," wldro " ] ,[ " a " ] ,[ " aa " ]]

----------------------------------------------------- EJERCICIO 2 ---------------------------------------------------------------

-- Define la función subsets tal que recibe una lista de elementos únicos y devuelve el conjunto potencia.
subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = [x:subset | subset <- remainingSubsets] ++ remainingSubsets
    where remainingSubsets = subsets xs

----------------------------------------------------- EJERCICIO 3 ---------------------------------------------------------------

-- El elemento mayoritario es el elemento que aparece más de ⌊n/2⌋ veces, donde n es la longitud de la lista. Define la
-- función majorityElem tal que recibe una lista y devuelve su elemento mayoritario.
-- La solución debe ser de complejidad O(n) en tiempo y O(1) en el espacio.
-- majorityElem :: Eq a => [a] -> a

----------------------------------------------------- EJERCICIO 4 ---------------------------------------------------------------

-- Define la función coins tal que recibe una lista de monedas de diferentes denominaciones y una cantidad total de dinero, 
-- y devuelve si es posible completar la cantidad usando únicamente ese tipo de monedas.
coins :: [Int] -> Int -> Bool
coins _ 0 = True  -- Si la cantidad es 0, siempre es posible
coins [] _ = False -- Si no hay monedas y la cantidad es distinta de 0, no es posible
-- Ahora viene el caso rescursivo, tenemso 2 opciones:
-- 1. Intentar restar la moneda actual (c) de la cantidad y continuar con las mismas monedas.
-- 2. No usar la moneda actual (c) y continuar con las monedas restantes (cs).
coins (c:cs) amount
    | amount < 0 = False
    | otherwise = coins (c:cs) (amount - c) || coins cs amount



-- Considera la siguiente definición de árbol binario:
data BST a = Empty | Node a (BST a) (BST a) deriving Show

-- Define la función isBST tal que recibe un árbol binario y devuelve si es un árbol de búsqueda binario válido. 
-- Un BST válido se define de la siguiente manera:
-- (a) El subárbol izquierdo contiene solo valores menores que la raíz. 
-- (b) El subárbol derecho contiene solo valores mayores que la raíz.
-- (c) Ambos subárboles deben ser árboles de búsqueda binarios.

----------------------------------------------------- EJERCICIO 5 ---------------------------------------------------------------

-- isBST :: BST Int -> Bool
-- isBST (Node x Empty Empty) = True
-- isBST (Node x Empty (Node y _ _)) = x < y && isBST (Node y _ _)
-- isBST (Node x (Node y _ _) Empty) = x > y && isBST (Node y _ _)
-- isBST (Node x (Node y _ _) (Node z _ _)) = x > y && x < z && isBST (Node y _ _) && isBST (Node z _ _)

----------------------------------------------------- EJERCICIO 6 ---------------------------------------------------------------

-- Define la función kthElem tal que recibe un árbol de búsqueda binaria y un número entero k, y devuelve el k-ésimo valor más pequeño.
-- kthElem :: BST a -> Int -> a
