--------------------------------------------------------------------
-- HASKELL PARSING LIBRARY                       
-- Roy Crole and Paula Severi 2025
--------------------------------------------------------------------


module Parse  where

import Tokens
import AST

--- Delete the comment from  next once you completed the code for that function 
-- infixr 5 `next`  
infixl 3 `build`
infixl 0 `alt`

---------------------
-- Type of parsers --
---------------------   

-- parser type: a parser takes a list of tokens; and returns either an error or a pair 

type Parse a  = Tokens -> Either Error (a,Tokens)

-------------------
-- Basic parsers --
-------------------


-- complete the code for key
key :: IMPword -> Parse IMPword
key kws (Key x : toks) = if kws==x then Right(x,toks) else Left  SyntaxError  -- ("Found a Key Token .. " ++ x ++ " .. but it does not match the input keyword/symbol"))
key kws _ = Left  SyntaxError  -- "Can't find the keyword/symbol .. "


-- complete the code for idr 
{--
idr :: Parse IMPword
idr (Id x : toks) = ???
idr toks = ???
--}

-- complete the code for num
{--
num :: Parse IMPword
???

--}

 -------------------------
-- Parsing combinators --
-------------------------


alt :: Parse a -> Parse a -> Parse a
alt ph1 ph2 toks = case ph1 toks of
                     Left _ -> ph2 toks
                     Right(r,toks') -> Right(r,toks')


-- complete the code for  next 
{--
next :: Parse a -> Parse b -> Parse (a,b)
next ph1 ph2 toks = case ph1 toks of
                    ???


--}

-- Repetition -

{-- Delete this comment when you completed next
 
many :: Parse a -> Parse [a] 
many ph  = (ph `next` many ph `build` cons) `alt` (\toks -> Right([], toks))
             where
             cons (x,xs) = x:xs
--}

-- Semantic action. The results from a parser ph are transformed by applying a function f.
build :: Parse a -> (a -> b) -> Parse b 
build ph f toks = case ph toks of
                    Left m  -> Left m
                    Right(r,toks') -> Right(f r, toks')
                    
                    
-- reader returns a function which maps an IMPfile
-- to a Haskell term of type a (eg BoolExp, Com, etc).
-- A parser p :: Parse a can be converted to such a
-- function:
-- Note there is an error if the input file is not fully consumed by ph 

{--
reader :: Parse a -> IMPFile -> Either Error a
reader ph impf
  = case ph (tokenize impf) of
      Right (result,[]) ->  Right result
      Right (result, s) -> Left SyntaxError
      Left  error   -> Left error

      --}
  


