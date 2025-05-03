
--------------------------------------------------------------------- 
-- SML EVALUATION SEMANTICS FOR IMP                                   
-- Roy Crole and Paula Severi 2025                                            
--------------------------------------------------------------------- 

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


{-- 
evalint :: IntExp -> State -> (Either Error Int) 

evalint (Int m) s = Right m 

evalint  (Var v) s =  ???

evalint  (IopExp (op,e1,e2))  s = do 
             m1 <-  evalint e1 s 
             m2 <-  evalint e2 s 
             Right (fromIop op m1 m2)
                                        
                                                      
                                     
--}

                                      
 ---------------------------------------------------------------------
-- Code the function evalbool for evaluating Boolean Expressions
-----------------------------------------------------------------------

{--
evalbool :: BoolExp -> State -> (Either Error Bool)
evalbool  (Bool b) s = Right b 
evalbool  (BopExp (op,e1,e2))  s =  ???
                                                       
--}
                                         
--------------------------------------------------------------------------
-- Code the function evalcom for evaluating Commands
---------------------------------------------------------------------------

{-- 
evalcom :: Com -> State -> (Either Error State) 


evalcom (Ass(v,e)) s = ???

evalcom (Seq(co1,co2))  s1  =  ???
                                      
        
evalcom (If(be,co1,co2)) s = ???
               

evalcom (While(be,co)) s1 = ???
          
          
          
evalcom (Repeat(co, be)) s1 = ???

--}
                                 
         
       
       
