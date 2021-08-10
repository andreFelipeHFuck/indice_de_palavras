import System.IO
import Data.Char
import Data.List



-- Aluno: André Felipe Fuck

-- HaskellTrabalho

-- Objetivo
{-
    Definir uma função que, dado um documento, gera um índice das 
    palavras que ocorrem nesse documento.
-}
 -- Numerar as linha s do documento:



-- Programa principal

 -- contruirIndice :: Doc -> [([Int], Palavra)]

-- Tipos 

type Doc = String 
type Linha = String 
type Palavra = String 


-- construirIndice :: Doc -> [([Int], Palavra)]
construirIndice :: IO ()
construirIndice = do 
     file_text <- readFile "texto.txt"
     let lower_file_text = map toLower file_text -- deixando todas as palavras em minusculo 
     let line_file_text = lines lower_file_text
     let hig_file_text = map higieniza line_file_text
     let num_linhas = numLinhas hig_file_text
     let num_palavras_bruto = map numeraPalavras num_linhas
     let num_palavras = foldr (++) [] num_palavras_bruto
     let ordenar_palavras = ordenar num_palavras
     let agrupar_palavras = agrupar ordenar_palavras
     let elinarRep_palavras = eliminarRep agrupar_palavras

     print elinarRep_palavras

-- Higienização 

higieniza ls =  [l | l <- ls, isAlpha l || isSpace l]

maior_3 l = repetidos [p | p <- words(l), length p >= 3]

numLinhas :: [b] -> [(Int, b)]
numLinhas ls =  zip [n | n <- [1 .. length ls]] [l | l <- ls]

numeraPalavras ls =  [(fst ls, p) | p <- maior_3 (snd ls)]

repetidos [] = []
repetidos (l: ls) = if l `elem` ls then repetidos ls else l: repetidos ls

--ordenar ls  = sortOn snd ls

--


menor_valor :: Ord a1 => (a2, a1) -> [(a2, a1)] -> (a2, a1)
menor_valor ini [] = ini
menor_valor ini (l: ls) = if snd ini < snd l then menor_valor ini ls else menor_valor l ls

menor :: Ord a1 => [(a2, a1)] -> (a2, a1)
menor l = menor_valor (head l) l


conta_removerElem :: (Eq a, Eq t, Num t) => t -> a -> [a] -> [a]
conta_removerElem _ _ [] = []
conta_removerElem c n (l:ls) | n == l && c == 0 = conta_removerElem (c + 1) n ls
                             | otherwise = l: conta_removerElem c n ls

removerElem n l = conta_removerElem 0 n l

ordenar [] = []
ordenar l = menor l: ordenar (removerElem (menor l) l)

--

agrupar ls = [([fst n | n <- ls, (snd p) == (snd n)], snd p)| p <- ls]

eliminarRep ls = repetidos ls

