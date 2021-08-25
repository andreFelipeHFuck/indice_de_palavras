-- Aluno: André Felipe Fuck

import System.IO
import Data.Char
import Data.List

-- Tipos 

type Doc = String 
type Linha = String 
type Palavra = String 

main :: IO ()
main = do putStr "Arquivo: "
          nome <- getLine
          txt <- readFile nome  
          putStrLn "Indice:\n"                      
          imprimir (contruirIndice txt)
         
contruirIndice :: Doc -> [([Int], Palavra)]
contruirIndice doc = do 
                      let lower_txt = map toLower doc -- Passando todas as palavras para letra minúscula
                      let line_file_text = lines lower_txt
                      let hig_file_text = map higieniza line_file_text
                      let num_linhas = numLinhas hig_file_text
                      let num_palavras = numeraPalavras num_linhas
                      let ordenar_palavras = ordenar num_palavras
                      let agrupar_palavras = agrupar ordenar_palavras
                      let elinarRep_palavras = eliminarRep agrupar_palavras
                      elinarRep_palavras

imprimir [] = putStrLn ""
imprimir ((n, l):ls) = do 
                       putStr l
                       putStr "............"
                       imprimir_lista n
                       imprimir ls

imprimir_lista [] = putStrLn ""
imprimir_lista (l: ls) = do
                         putStr (show l)
                         putStr " "
                         imprimir_lista ls

higieniza :: [Char] -> [Char]
higieniza ls =  [l | l <- ls, isAlpha l || isSpace l]

maior_3 l =  repetidos [p | p <- words(l), length p >= 3]

numLinhas :: [Linha] -> [(Int, Linha)]
numLinhas ls =  zip [n | n <- [1 .. length ls]] [l | l <- ls]

numeraPalavras :: [(Int, Linha)] -> [(Int, Palavra)]
numeraPalavras ls = foldr (++) [] (numeraPalavras'' ls)

numeraPalavras' ls =  [(fst ls, p) | p <- maior_3 (snd ls)]

numeraPalavras'' ls = map numeraPalavras' ls
 
repetidos [] = []
repetidos (l: ls) = if l `elem` ls then repetidos ls else l: repetidos ls


ordenar :: [(Int, Palavra)] -> [(Int, Palavra)]
ordenar ls = sortOn snd ls

agrupar :: [(Int, Palavra)] -> [([Int], Palavra)]
agrupar [] = []
agrupar (l:ls) = ind (snd l) (l:ls) : agrupar (repetidos' (snd l) ls)

repetidos' _ [] = []
repetidos' p (l:ls) = if p == (snd l) then repetidos' p ls else l: repetidos' p ls

ind p ls = ([fst n | n <- ls, p == (snd n)], p)

eliminarRep ::[([Int], Palavra)] -> [([Int], Palavra)]
eliminarRep [] = []
eliminarRep (l:ls) = (repetidos (fst l), snd l) : eliminarRep ls
