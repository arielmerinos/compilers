-- PRÁCTICA: Análisis Sintáctico

-- :::::::::::::: INTEGRANTES DEL EQUIPO ::::::::::::::

-- Aquino Chapa Armando Abraham | 317058163 | @ArmandoAAC
-- Bonilla Ruiz Roberto Adrian | 317219038 | @boruroad
-- Cristóbal Morales Karen | 317180321 | @NerakCM
-- García Toxqui Demian Oswaldo | 317088296 | @DemianGT
-- Merino Pena Kevin Ariel | 317031326 | @arielmerinos

----------------------------------------------------------------------------

-- Considera el siguiente lenguaje conocido como WHILE:
--      Comp C ::= L := E | if B then C else C | (C; C) | while B do C | skip
--      Bool B ::= true | false | E = E | &BB | − B
--      Arith E ::= L | n | (E + E)

-- El objetivo de esta practica es profundizar en el proceso de análisis sintáctico	. La práctica consiste en definir las siguientes funciones:

-- Considera la siguiente definición de Tokens:

data Token = Assign | If | Then | Else | Seq | While | Do | Skip | Boolean Bool | Equal | And | Not | Loc Int | Number Int | LP | RP | Sum deriving Show

data Content = T Token | S | C | B | E deriving Show
type Input = [ Token ]
type Stack = [ Content ]

----------------------------------------------------- EJERCICIO 1 ---------------------------------------------------------------
-- :- 4.9 pts Define la función parserAux que recibe una lista de tokens, el stack y devuelve verdadero si y solo si la lista de
--            tokens pertenece al lenguaje con el stack actual. Esta función es un parser LL(1) ad hoc del lenguaje.

parserAux :: Input -> Stack -> Bool
parserAux [][] = True
-- Loc
parserAux (Loc x:xs)(T (Loc y):ys) = parserAux xs ys -- Loc = Loc en el stack
parserAux (Loc x:xs)(S:ys) = parserAux (Loc x:xs)(C:ys) -- 1
parserAux (Loc x:xs)(C:ys) = parserAux (Loc x:xs)(T (Loc x): T Assign: E: ys) -- 2
parserAux (Loc x:xs)(B:ys) = parserAux (Loc x:xs)(E: T Equal: E: ys) --9
parserAux (Loc x:xs)(E:ys) = parserAux (Loc x:xs)(T (Loc x) :ys) -- 12
-- If
parserAux (If:xs)(T If:ys) = parserAux xs ys -- If = If en el stack
parserAux (If:xs)(S:ys) = parserAux (If:xs)(C:ys) -- 1
parserAux (If:xs)(C:ys) = parserAux (If:xs)(T If: B: T Then: C: T Else: C: ys) -- 3
-- While
parserAux (While:xs)(T While: ys) = parserAux xs ys -- While = While en el stack
parserAux (While:xs)(S:ys) = parserAux (While:xs)(C:ys) -- 1
parserAux (While:xs)(C:ys) = parserAux (While:xs)(T While: B: T Do: C: ys) -- 5
-- Skip
parserAux (Skip:xs)(T Skip: ys) = parserAux xs ys -- Skip = Skip en el stack
parserAux (Skip:xs)(S:ys) = parserAux (Skip:xs)(C:ys) -- 1
parserAux (Skip:xs)(C:ys) = parserAux (Skip:xs)(T Skip:ys) -- 6
--LP
parserAux (LP:xs)(T LP: ys) = parserAux xs ys -- LP = LP en el stack
parserAux (LP:xs)(S:ys) = parserAux (LP:xs)(C:ys) -- 1
parserAux (LP:xs)(C:ys) = parserAux (LP:xs)(T LP: C: T Seq: C: T RP: ys) -- 4
parserAux (LP:xs)(B:ys) = parserAux (LP:xs)(E: T Equal: E: ys) --9
parserAux (LP:xs)(E:ys) = parserAux (LP:xs)(T LP: E: T Sum: E: T RP: ys) -- 14
--RP
parserAux (RP:xs)(T RP: ys) = parserAux xs ys -- RP = RP en el stack
-- Number
parserAux (Number x:xs)(T (Number y):ys) = parserAux xs ys -- n = n en el stack
parserAux (Number x:xs)(B:ys) = parserAux (Number x:xs)(E: T Equal: E: ys) --9
parserAux (Number x:xs)(E:ys) = parserAux (Number x:xs)(T (Number x) :ys) -- 13
-- true
parserAux (Boolean True:xs)(T (Boolean True):ys) = parserAux xs ys -- True = True en el stack
parserAux (Boolean True:xs)(B:ys) = parserAux ((Boolean True):xs)(T (Boolean True): ys) --7
-- false
parserAux (Boolean False:xs)(T (Boolean False):ys) = parserAux xs ys -- False = False en el stack
parserAux (Boolean False:xs)(B:ys) = parserAux ((Boolean False):xs)(T (Boolean False): ys) --8
-- and
parserAux (And:xs)(T And:ys) = parserAux xs ys -- And = And en el stack
parserAux (And:xs)(B:ys) = parserAux (And:xs)(T And: B: B: ys) --10
-- not
parserAux (Not:xs)(T Not:ys) = parserAux xs ys -- Not = Not en el stack
parserAux (Not:xs)(B:ys) = parserAux (Not:xs)(T Not: B: ys) --11
-- equal 
parserAux (Equal:xs)(T Equal:ys) = parserAux xs ys -- Equal = Equal en el stack
-- then
parserAux (Then:xs)(T Then:ys) = parserAux xs ys -- Then = Then en el stack
-- Else
parserAux (Else:xs)(T Else:ys) = parserAux xs ys -- Else = Else en el stack
-- Seq
parserAux (Seq:xs)(T Seq:ys) = parserAux xs ys -- Seq = Seq en el stack
-- Do
parserAux (Do:xs)(T Do:ys) = parserAux xs ys -- Do = Do en el stack
-- Sum
parserAux (Sum:xs)(T Sum:ys) = parserAux xs ys -- Sum = Sum en el stack
-- Assign
parserAux (Assign:xs)(T Assign:ys) = parserAux xs ys -- Sum = Sum en el stack
-- Caso de salida negativo
parserAux (x:xs)_ = False
parserAux [] _ = False


-- :- 0.1 pts Utilizando la función parserAux, define la función parser que recibe una lista de tokens WHILE y devuelve verdadero 
--            si y solo si la lista de tokens pertenece al lenguaje.
parser :: Input -> Bool
parser x = parserAux x [S]