--------------------------------------------------------------------- 
-- SML EVALUATION SEMANTICS FOR IMP                                   
-- Roy Crole and Paula Severi 2025                                            
--------------------------------------------------------------------- 
module EvSem

where

import AST
import Basic


fromIop  :: Iop -> (Int -> Int -> Int)
fromIop Plus  = (+)
fromIop Minus = (-)
fromIop Times = (*) 

fromBop  :: Bop -> (Int -> Int -> Bool)
fromBop Le = (<)
fromBop Gr = (>)
fromBop LeEq = (<=) 
fromBop GrEq = (>=)

evalint :: IntExp -> State -> (Either Error Int) 
evalint (Int m) s = Right m 

evalint (Var v) s = lookUp s v

evalint (IopExp (op,e1,e2)) s = do 
    m1 <- evalint e1 s 
    m2 <- evalint e2 s 
    Right (fromIop op m1 m2)
                                      
evalbool :: BoolExp -> State -> (Either Error Bool)
evalbool (Bool b) s = Right b 
evalbool (BopExp (op,e1,e2)) s = do
    m1 <- evalint e1 s
    m2 <- evalint e2 s
    Right (fromBop op m1 m2)
                                         
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