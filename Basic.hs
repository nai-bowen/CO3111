 
------------------------------------------------------------------- 
-- HASKELL BASIC LIST AND STRING PROCESSING FUNCTIONS           
-- Roy L. Crole and Paula Severi 2025                                      
------------------------------------------------------------------- 

module Basic where

import AST

-- code mem 

{-- 
mem :: Eq a => a -> [a] -> Bool
mem x []     = ???
mem x (h:t)  = ???
--}

-- code lookUp

{-- 
lookUp :: Eq a => [(a,b)] -> a -> Either Error b
lookUp [] v             = ???
lookUp ((x,y):pairs) v  = ???
--}

-- code update

{--
update :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
update [] v z = ???
update ((v,z):pairs) v' z'  = ???
--}
