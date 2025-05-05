
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

-- Checking if a character is a letter
is_letter :: Char -> Bool
is_letter c = 'A'<=c && c<='Z' || 'a'<=c && c<='z'

-- Checking if a character is a digit
is_digit :: Char -> Bool
is_digit c = '0'<=c && c<='9'

is_neg c = '-' == c 

specials = ",!@#$%^&*()_-+=|[]:;'~`<>.?/"

-- Check if a character is a special symbol
is_special c = c `mem` specials

-- Takes as many letters in sequence from IMPfile as possible
alpha :: (String, IMPFile) -> (String, IMPFile)
alpha (al, c:cs) = if is_letter c then alpha(al++[c],cs) else (al, c:cs)
alpha (al,[]) = (al, [])

-- Takes as many digits in sequence from IMPfile as possible
numeric :: (String, IMPFile) -> (String, IMPFile)
numeric (nu, c:cs) = if is_digit c then numeric(nu++[c],cs) else (nu, c:cs)
numeric (nu,[]) = (nu, [])

-- Takes as many special symbols in sequence from IMPfile as possible (up to 2)
symbolic :: (String, IMPFile) -> (String, IMPFile)
symbolic (sy, c:cs) = 
  if not (is_special c) then (sy, c:cs) else
    if (sy++[c]) `mem` symbols
    then
      symbolic (sy++[c], cs)
    else (sy, c:cs)
symbolic (sy, []) = (sy, [])

-- Scan the input and convert it to tokens
scanning :: (Tokens, IMPFile) -> Tokens 
scanning (toks, []) = toks
scanning (toks, c:cs) = 
      if 
        is_letter c
      then
        let (al, cs2) = alpha([c],cs) in 
        if al `mem` keywords then scanning (toks++[Key al],cs2) else scanning (toks++[Id al],cs2)
      else 
        if is_digit c then
          let (nu, cs2) = numeric([c],cs) in scanning (toks++[Num nu],cs2)
        else 
          if is_special c then
            let (sy, cs2) = symbolic([c],cs) in scanning (toks++[Key sy],cs2)
          else
            scanning (toks,cs)
            
-- Main function to convert an IMPFile to Tokens
tokenize :: IMPFile -> Tokens 
tokenize impf = scanning([], impf)