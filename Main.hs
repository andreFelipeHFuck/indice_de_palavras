import System.IO
import Data.Char
import Text.PrettyPrint.Annotated.HughesPJClass (Doc)


-- Aluno: André Felipe Fuck

-- HaskellTrabalho

-- Objetivo
{-
    Definir uma função que, dado um documento, gera um índice das 
    palavras que ocorrem nesse documento.
-}


-- Programa principal

 -- contruirIndice :: Doc -> [([Int], Palavra)]

contruirIndice :: IO ()
contruirIndice = do 
     file_text <- readFile "texto.txt"
     let lower_file_text = map toLower file_text -- deixando todas as palavras em minusculo 
     let higeniza_file_text = higieniza lower_file_text
     let line_file_text = lines higeniza_file_text
     print line_file_text

-- Higienização 

higieniza l =  [x | x <- l, isAlpha x || isSpace x]
    
    -- [y | y <- l, isSpace y] 