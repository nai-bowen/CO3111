
---------------------------------------------------------------------------
-- HASKELL TOKENS FOR EXPRESSIONS AND COMMANDS FOR IMPERATIVE LANGUAGE IMP                          
-- Roy L. Crole and Paula Severi 2025                                          
---------------------------------------------------------------------------


module Tokens where

import Basic

type IMPFile = String
type IMPword = String
type IMPwords = [IMPword]

-- a token is an identifier, keyword or integer

data Token = Id IMPword | Key IMPword | Num IMPword
             deriving (Show, Eq)
             
type Tokens = [Token]


--- Keywords 

keywords  :: IMPwords
keywords = ["true","false","while","do","if","then","else","run","eval","trans","repeat","until", "quit","C","E"]

-- Special symbols 

symbols :: IMPwords
symbols = ["(",")","+","-","*","<=",">=","<",">",";",":=","[",",","]","[]"]

-- complete the code for is_letter
{--
is_letter :: Char -> Bool
is_letter c = 'A'<=c && c<='Z' || ???
--}

-- complete the code for is_digit
{--
is_digit c = '0'<=c && ??
--}

is_neg c = '-' == c 

specials = ",!@#$%^&*()_-+=|[]:;'~`<>.?/"

-- delete the comment from is_special if you have already coded mem in Basics.hs

{-- 
is_special c = c `mem` specials
--}


-- complete the code for the function alpha
{--
alpha :: (String, IMPFile) -> (String, IMPFile)
alpha (al, c:cs) = if is_letter c then alpha(al++[c],cs) else (al, c:cs)
alpha (al,[]) = ???
--}


-- complete the code for the function numeric
{--
numeric :: (String, IMPFile) -> (String, IMPFile)
numeric (nu, c:cs) = ???
numeric (nu,[]) = ???
--}

-- complete the code for symbolic
{--
symbolic :: (String, IMPFile) -> (String, IMPFile)
symbolic (sy, c:cs) = 
  if ??? then ??? else
    if
      (sy++[c]) `mem` ???
    then
      symbolic (???, ???)
    else ???
symbolic (sy, []) = (sy, [])

--}

----------------------------------
-- complete the code for scanning
-----------------------------------
{--
scanning :: (Tokens, IMPFile) -> Tokens 
scanning (toks, []) = toks
scanning (toks, c:cs) = 
      if 
        is_letter c
      then
        let (al, cs2) = alpha([c],cs) in 
        if al `mem` keywords then scanning (toks++[Key al],cs2) else scanning (toks++[Id al],cs2)
      else 
        if is_digit c  then
          ???
        else 
          if ??? then
            ???
          else
              scanning (toks,cs)
 --}           


-- Delete the comments for tokenize once you code scanning
{--
tokenize :: IMPFile -> Tokens 
tokenize impf = scanning([], impf)
--}


