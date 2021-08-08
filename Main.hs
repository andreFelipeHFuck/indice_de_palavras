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

ordenar ls  = sortOn snd ls

agrupar ls = [([fst n | n <- ls, (snd p) == (snd n)], snd p)| p <- ls]

eliminarRep ls = repetidos ls
