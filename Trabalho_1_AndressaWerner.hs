module Main where

-- Aqui estou importando algumas funções para transformar de inteiros para caracteres
--  e vice-vesa, funções de entrada/saída e números aleatóreos:

import Data.Char
import System.IO
import System.Random

-- Tabuleiro do jogo:
type GBoard = [[Char]]
-- Tabuleiro que contem a posicao das minas (Mapa de Minas). True = mina, False = sem mina:
type MBoard = [[Bool]]

-- -- Exemplo de Tabuleiro 9x9 inicial todo fechado:
-- gBoard :: GBoard
-- gBoard = [['-','-','-','-','-','-','-','-','-'],
--           ['-','-','-','-','-','-','-','-','-'],
--           ['-','-','-','-','-','-','-','-','-'],
--           ['-','-','-','-','-','-','-','-','-'],
--           ['-','-','-','-','-','-','-','-','-'],
--           ['-','-','-','-','-','-','-','-','-'],
--           ['-','-','-','-','-','-','-','-','-'],
--           ['-','-','-','-','-','-','-','-','-'],
--           ['-','-','-','-','-','-','-','-','-']]

-- -- Exemplo de tabuleiro 9x9 com a posição das minas:

-- mBoard :: MBoard
-- mBoard = [[False, False, False, False, False, False, False, False, False],
--           [False, False, False, False, False, False, False, False, False],
--           [False, False, False, False, False, False, False, False, False],
--           [False, False, False, False, False, False, False, False, False],
--           [False, False, False, False, True , False, False, False, False],
--           [False, False, False, False, False, True, False, False, False],
--           [False, False, False, False, False, False, False, False, False],
--           [False, False, False, False, False, False, False, False, False],
--           [False, False, False, False, False, False, False, False, False]]


-- PRIMEIRA PARTE - FUNÇÕES PARA MANIPULAR OS TABULEIROS DO JOGO (MATRIZES)

-- A ideia das próximas funções é permitir que a gente acesse uma lista usando um indice,
-- como se fosse um vetor

-- gArr (get array): recebe uma posicao (p) e uma lista (vetor) e devolve o elemento
-- na posição p do vetor

gArr :: Int -> [t] -> t
gArr a [] = error "Lista Vazia"
gArr a (x:xs)
    | a == 0 = x
    | otherwise = gArr (a-1) xs

-- uArr (update array): recebe uma posição (p), um novo valor (v), e uma lista (vetor) e devolve um
-- novo vetor com o valor v na posição p 

uArr :: Int -> a -> [a] -> [a]
uArr p v (x:xs)
    | p == 0 = [v] ++ xs
    | otherwise = [x] ++ uArr (p-1) v xs

-- gPos (get position) recebe linha (l), coluna (c) (não precisa validar) e um tabuleiro. Devolve o elemento na posicao
-- tabuleiro[l,c]. Usar gArr na implementação

gPos :: Int -> Int -> [[a]] -> a
gPos l c mapa = gArr c (gArr l mapa)

-- uPos (update position): recebe um novo valor, uma posição no tabuleiro (linha e coluna) e um tabuleiro. Devolve 
-- o tabuleiro modificado com o novo valor na posiçao lxc

uPos :: Int -> Int -> a -> [[a]] -> [[a]]
uPos l c v (y:ys) -- = uArr c v (getArray l [[y]])
    | l == 0 = [uArr c v y] ++ ys
    | otherwise = [y] ++ uPos (l-1) c v ys

--------------- SEGUNDA PARTE: LÓGICA DO JOGO

-- isMine: recebe linha coluna e o tabuleiro de minas, e diz se a posição contém uma mina

isMine :: Int -> Int -> MBoard -> Bool
isMine l c mapa
    | gPos l c mapa == True = True
    | otherwise = False

-- isValidPos: recebe o tamanho do tabuleiro (ex, em um tabuleiro 9x9, o tamanho é 9), 
-- uma linha e uma coluna, e diz se essa posição é válida no tabuleiro

isValidPos :: Int -> Int -> Int -> Bool
isValidPos tam l c
    | c >= tam = False
    | l >= tam = False
    | c < 0 = False
    | l < 0 = False
    | otherwise = True

-- validMoves: Dado o tamanho do tabuleiro e uma posição atual (linha e coluna), retorna uma lista
-- com todas as posições adjacentes à posição atual

-- Exemplo: Dada a posição linha 3, coluna 3, as posições adjacentes são: [(2,2),(2,3),(2,4),(3,2),(3,4),(4,2),(4,3),(4,4)]
-- ...   ...      ...    ...   ...
-- ...  (2,2)    (2,3)  (2,4)  ...
-- ...  (3,2)    (3,3)  (3,4)  ...
-- ...  (4,2)    (4,3)  (4,4)  ...
-- ...   ...      ...    ...   ...

--  Dada a posição (0,0) que é um canto, as posições adjacentes são: [(0,1),(1,0),(1,1)]

--  (0,0)  (0,1) ...
--  (1,0)  (1,1) ...
--   ...    ...  ...

casosDeTeste :: Int -> Int -> [(Int,Int)]
casosDeTeste l c = [(l-1, c-1), (l-1, c), (l-1, c+1), (l, c-1), (l, c+1), (l+1, c-1), (l+1, c+1), (l+1, c)]

testandoMoves :: Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)] 
testandoMoves tam l c [] = []
testandoMoves tam l c ((a,b):xs)
    | isValidPos tam a b == True = (a,b) : testandoMoves tam l c xs
    | otherwise = testandoMoves tam l c xs

validMoves :: Int -> Int -> Int -> [(Int,Int)]
validMoves tam l c = testandoMoves tam l c (casosDeTeste l c)

-- cMinas: recebe uma posicao  (linha e coluna), o tabuleiro com o mapa das minas, e conta quantas minas
-- existem nas posições adjacentes

contaTamanho :: [t] -> Int
contaTamanho [] = 0
contaTamanho (x:xs) = 1 + contaTamanho xs


contaAsMinas :: MBoard -> [(Int, Int)] -> Int
contaAsMinas mapa [] = 0
contaAsMinas mapa ((b,c):xs)
    | gPos b c mapa == True = 1 + contaAsMinas mapa xs
    | otherwise = contaAsMinas mapa xs

cMinas :: Int -> Int -> MBoard -> Int
cMinas l c mapa = contaAsMinas mapa (validMoves (contaTamanho mapa) l c)
        

--- abreJogada: é a função principal do jogo!!
--- recebe uma posição a ser aberta (linha e coluna), o mapa de minas e o tabuleiro do jogo. Devolve como
--  resposta o tabuleiro do jogo modificado com essa jogada.
--- Essa função é recursiva, pois no caso da entrada ser uma posição sem minas adjacentes, o algoritmo deve
--- seguir abrindo todas as posições adjacentes até que se encontre posições adjacentes à minas.
--- Vamos analisar os casos:
--- - Se a posição a ser aberta é uma mina, o tabuleiro não é modificado e encerra
--- - Se a posição a ser aberta já foi aberta, o tabuleiro não é modificado e encerra
--- - Se a posição a ser aberta é adjacente a uma ou mais minas, devolver o tabuleiro modificado com o número de
--- minas adjacentes na posição aberta
--- - Se a posição a ser aberta não possui minas adjacentes, abrimos ela com zero (0) e recursivamente abrimos
--- as outras posições adjacentes a ela


abreJogadas :: [(Int, Int)] -> MBoard -> GBoard -> GBoard
abreJogadas [] mapa tabuleiro = tabuleiro
abreJogadas ((l, c):xs) mapa tabuleiro = abreJogada l c mapa (abreJogadas xs mapa tabuleiro)


abreJogada :: Int -> Int -> MBoard -> GBoard -> GBoard
abreJogada l c mapa tabuleiro
    | gPos l c mapa == True = uPos l c '*' tabuleiro -- é uma mina, o tabuleiro não é modificado e encerra
    | gPos l c tabuleiro /= '-' = tabuleiro -- já foi aberta, o tabuleiro não é modificado e encerra
    | cMinas l c mapa > 0 = uPos l c (intToDigit(cMinas l c mapa)) tabuleiro -- maior que zero
    | otherwise = abreJogadas (validMoves (contaTamanho mapa) l c) mapa (uPos l c '0' tabuleiro)

-- - abreTabuleiro: recebe o mapa de Minas e o tabuleiro do jogo, e abre todo o tabuleiro do jogo, mostrando
-- - onde estão as minas e os números nas posições adjecentes às minas. Essa função é usada para mostrar
-- - todo o tabuleiro no caso de vitória ou derrota

abreTabuleiro :: MBoard -> GBoard -> GBoard
abreTabuleiro mapa tabuleiro
    | contaFechadas tabuleiro == 0 = tabuleiro
    | contaFechadas tabuleiro == 1 = abreJogada (fst(localizaFechada mapa tabuleiro)) (snd(localizaFechada mapa tabuleiro)) mapa tabuleiro
    | otherwise = abreJogadas (todasJogadas (contaTamanho tabuleiro)) mapa tabuleiro

todasJogadas :: Int -> [(Int,Int)]
todasJogadas 0 = []
todasJogadas 1 = [(0,0)]
todasJogadas x = jogadasAtuais x x ++ todasJogadas (x-1)

jogadasAtuais :: Int -> Int -> [(Int,Int)]
jogadasAtuais x 1 = [(x-1, 0), (0, (x-1))]
jogadasAtuais x y
    | x == y = (x-1, y-1) : jogadasAtuais x (y-1) 
    | otherwise = (x-1, y-1) : (y-1, x-1) : jogadasAtuais x (y-1) 

localX :: GBoard -> Int
localX (x:xs)
    | contaArray x '-' > 0 = 0
    | otherwise = 1 + localX xs

localY :: Int -> GBoard -> Int
localY a tabuleiro = localizaY (gArr a tabuleiro)

localizaY :: [Char] -> Int
localizaY (x:xs)
    | x == '-' = 0
    | otherwise = 1 + localizaY xs


localizaFechada :: MBoard -> GBoard -> (Int,Int)
localizaFechada mapa tabuleiro = (localX tabuleiro, localY (localX tabuleiro) tabuleiro)


--  -- contaFechadas: Recebe um GBoard e conta quantas posições fechadas existem no tabuleiro (posições com '-')

contaArray :: [Char] -> Char -> Int
contaArray [] a = 0
contaArray (y:ys) a
    | y == a = 1 + contaArray ys a 
    | otherwise = contaArray ys a


contaFechadas :: GBoard -> Int
contaFechadas [] = 0
contaFechadas (x:xs) = contaArray x '-' + contaFechadas xs


-- contaMinas: Recebe o tabuleiro de Minas (MBoard) e conta quantas minas existem no jogo
contaBool :: [Bool] -> Bool -> Int
contaBool [] a = 0
contaBool (y:ys) a
    | y == a = 1 + contaBool ys a 
    | otherwise = contaBool ys a

contaMinas :: MBoard -> Int
contaMinas [] = 0
contaMinas (x:xs) = contaBool x True + contaMinas xs



-- endGame: recebe o tabuleiro de minas, o tauleiro do jogo, e diz se o jogo acabou.
-- O jogo acabou quando o número de casas fechadas é igual ao numero de minas

endGame :: MBoard -> GBoard -> Bool
endGame a b
    | contaFechadas b == contaMinas a = True
    | otherwise = False

---
---  PARTE 3: FUNÇÕES PARA GERAR TABULEIROS E IMPRIMIR TABULEIROS
---

-- printBoard: Recebe o tabuleiro do jogo e devolve uma string que é a representação visual desse tabuleiro
-- Usar como referncia de implementacao o video sobre tabela de vendas (Aula 06)

printBoard :: GBoard -> String
printBoard [] = ""
printBoard (x:xs) = linha x ++ "\n" ++ printBoard xs

linha :: [Char] -> String
linha [] = ""
linha (x:xs) = "" ++ show x ++ linha xs

-- geraLista: recebe um inteiro n, um valor v, e gera uma lista contendo n vezes o valor v

geraLista :: Int -> a -> [a]
geraLista 0 a = []
geraLista x a = a : geraLista (x-1) a

-- geraTabuleiro: recebe o tamanho do tabuleiro e gera um tabuleiro  novo, todo fechado (todas as posições
-- contém '-'). A função geraLista deve ser usada na implementação

geraNovoTabuleiro :: Int -> GBoard
geraNovoTabuleiro 0 = []
geraNovoTabuleiro a = geraLista a (geraLista a '-') 


-- geraMapaDeMinasZerado: recebe o tamanho do tabuleiro e gera um mapa de minas zerado, com todas as posições
-- contendo False. Usar geraLista na implementação

geraMapaDeMinasZerado :: Int -> MBoard
geraMapaDeMinasZerado 0 = []
geraMapaDeMinasZerado a = geraLista a (geraLista a False)

-- A função a seguir (main) deve ser substituida pela função main comentada mais
-- abaixo quando o jogo estiver pronto

--main :: IO ()
--main = print "Alo Mundo!"



-- Aqui está o Motor do Jogo.
-- Essa parte deve ser descomentada quando as outras funções estiverem implementadas
-- Para rodar o jogo, digite "main" no interpretador

main :: IO ()
main = do
   putStr "Digite o tamanho do tabuleiro: "
   size <- getLine
   mb <- genMinesBoard (read size)
   gameLoop mb (geraNovoTabuleiro (read size)) 

gameLoop :: MBoard -> GBoard -> IO ()
gameLoop mb gb = do
   putStr (printBoard gb)
   putStr "Digite uma linha: "
   linha <- getLine
   putStr "Digite uma coluna: "
   coluna <- getLine
   if (isMine (read linha) (read coluna) mb)
      then do
            putStr "VOCE PERDEU!\n"
            putStr $ printBoard $ abreTabuleiro mb gb
            putStr "TENTE NOVAMENTE!\n"
      else do
            let newGB = (abreJogada (read linha) (read coluna) mb gb)
            if (endGame mb newGB)
                 then do
                     putStr "VOCE VENCEU!!!!!!!!\n"
                     putStr $ printBoard $ abreTabuleiro mb newGB
                     putStr "PARABENS!!!!!!!!!!!\n"
                 else
                     gameLoop mb newGB




----- DO NOT GO BEYOUND THIS POINT   


genMinesBoard :: Int -> IO MBoard
genMinesBoard size = do
        board <- addMines (round   ((fromIntegral (size *size)) * 0.15)) size (geraMapaDeMinasZerado size) 
        return board

addMines :: Int -> Int -> MBoard -> IO MBoard
addMines 0 size b = return b
addMines n size b = do
                l <- randomRIO (0,(size-1))
                c <- randomRIO (0,(size-1))
                case isMine l c b of
                      True -> addMines n size b
                      False -> addMines (n-1) size (uPos l c True b)

