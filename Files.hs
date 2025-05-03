 
---------------------------------------------------------------------
-- MAIN FILE: HASKELL USERS' FUNCTIONS FOR IMP                                 
-- Roy L. Crole and Paula Severi 2025                                              
---------------------------------------------------------------------

module Files  where

import System.Directory

import AST
-- import Basic
import Pretty 
import Tokens
import Parse
import ImpPar 
import EvSem


-- Helper functions for formatting evaluation results

arrow :: String
arrow = " ==> "
 
formatValue :: Show a => [Char] -> a -> [Char]
formatValue x v = x  ++ arrow  ++ show v

formatError :: Pretty a => [Char] -> a -> [Char]
formatError x err =  x ++ "\n*** " ++ pp err
  
format :: (Show b, Pretty a) => String -> Either a b   -> String
format x (Right v)  = formatValue x v 
format x (Left err) = formatError x err
  
  
-- Process each line

{-- Complete the following functions  

-- Complete the following function that processes one line of the file 

processLine :: String -> String
processLine "" = ""
processLine x =
    case reader ???  ???  of
        Right (C c, s)  -> format x ???
        Right (E e, s)  -> format x ???
        Left err        -> formatError x err 

-- The processLines function applies the previous transformation processLine to each line of a file's contents 

processLines:: [String] -> [String]
processLines =  ???
   
--The processAll function takes a string representing the contents of a file, splits it into a list of lines, processes each --line using processLines, and then joins the processed lines back into a single string.

processAll :: String -> String
processAll = ??? 

-- Complete processFile

processFile :: FilePath -> IO()
processFile x =  do  existsFile <-  doesFileExist x 
                     if existsFile then 
                        do filestr <- ???
                           ??? 
                     else putStrLn "The file does not exist" 

--}

