module Screen where

import AST
import Basic
import Pretty 
import Tokens
import Parse
import ImpPar 
import EvSem
import Files

-- Format the result of evaluating a program
formatEval :: Prog -> String
formatEval (C c, s) = format "" (evalcom c s)
formatEval (E e, s) = format "" (evalint e s)

-- Interactive prompt
prompt :: IO()
prompt = do 
    putStr "\n >IMP> \n"
    input <- getLine
    if input == "" then 
        prompt 
    else
        case readins input of
            Right (Run file) -> do
                processFile file
                prompt
            Right (Eval p) -> do
                putStr (formatEval p)
                prompt
            Right Quit -> return ()
            Left error -> do
                putStrLn (formatError "" error)
                prompt