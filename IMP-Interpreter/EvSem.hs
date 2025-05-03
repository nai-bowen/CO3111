module EvSem

where

import AST
import Basic

-------------------------------------------------------------------
-- Helper functions: Converting  the constructors for operators into real functions 
--  You could use these functions to make the code shorter
-------------------------------------------------------------------

fromIop  :: Iop -> (Int -> Int -> Int)
fromIop Plus  = (+)
fromIop Minus = (-)
fromIop Times = (*) 

fromBop  :: Bop -> (Int -> Int -> Bool)
fromBop Le = (<)
fromBop Gr = (>)
fromBop LeEq = (<=) 
fromBop GrEq = (>=)


----------------------------------------------------------------
--  Code the function evalint for evaluating Integer Expressions
----------------------------------------------------------------

evalint :: IntExp -> State -> (Either Error Int) 
evalint (Int m) s = Right m 

evalint (Var v) s = lookUp s v

evalint (IopExp (op,e1,e2)) s = do 
    m1 <- evalint e1 s 
    m2 <- evalint e2 s 
    Right (fromIop op m1 m2)
                                      
 ---------------------------------------------------------------------
-- Code the function evalbool for evaluating Boolean Expressions
-----------------------------------------------------------------------

evalbool :: BoolExp -> State -> (Either Error Bool)
evalbool (Bool b) s = Right b 
evalbool (BopExp (op,e1,e2)) s = do
    m1 <- evalint e1 s
    m2 <- evalint e2 s
    Right (fromBop op m1 m2)
                                         
--------------------------------------------------------------------------
-- Code the function evalcom for evaluating Commands
---------------------------------------------------------------------------

evalcom :: Com -> State -> (Either Error State) 
evalcom (Ass(v,e)) s = do
    val <- evalint e s
    Right (update s v val)

evalcom (Seq(co1,co2)) s1 = do
    s2 <- evalcom co1 s1
    evalcom co2 s2
                                      
evalcom (If(be,co1,co2)) s = do
    b <- evalbool be s
    if b then evalcom co1 s else evalcom co2 s
               
evalcom (While(be,co)) s1 = do
    b <- evalbool be s1
    if b 
    then do
        s2 <- evalcom co s1
        evalcom (While(be,co)) s2
    else Right s1
          
evalcom (Repeat(co, be)) s1 = do
    s2 <- evalcom co s1
    b <- evalbool be s2
    if b
    then Right s2
    else evalcom (Repeat(co, be)) s2