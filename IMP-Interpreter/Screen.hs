 
---------------------------------------------------------------------
-- MAIN FILE: HASKELL USERS' FUNCTIONS FOR IMP                                 
-- Roy L. Crole and Paula Severi 2025                                              
---------------------------------------------------------------------

module Screen  where


import AST
-- import Basic
import Pretty 
import Tokens
import Parse
import ImpPar 
import EvSem
import Files


-- Complete the following helper function to handle Eval cases
{--
formatEval :: Prog -> String
formatEval (C c, s) = format "" ???
formatEval (E e, s) = format "" ???

--}

--------------------------------------------------------------------- -----------
-- Complete prompt 
----------------------------------------------------------------------------------

{--

prompt :: IO()
prompt = do putStr "\n >IMP> \n"
            input <- getLine
            if input == "" then prompt else
             case ??? of
              Right (Run  file) ->  ???
              Right (Eval p)   -> putStr (formatEval p) >> ???
              Right Quit      ->  ???
              Left error  -> putStrLn (formatError "" error)  >> prompt


--}


