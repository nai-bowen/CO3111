 
---------------------------------------------------------------------
-- MAIN FILE: HASKELL USERS' FUNCTIONS FOR IMP                                 
-- Roy L. Crole and Paula Severi 2025                                            
---------------------------------------------------------------------

module Main where


import AST
import Basic
import Pretty 
import Tokens
import Parse
import ImpPar 
import EvSem
import Files 
import Screen

--------------------------
--- Welcoming message -----
---------------------------

introduction =  [
  " ****  Welcome to the world of IMP  ***** ",
 "Copyright (c) Leicester University, 2025" ]

------------------------
--  main 
-------------------------

main :: IO()
main = do  (putStr . unlines) introduction
           prompt 
           
          
