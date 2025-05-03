module TestTokens where

import Test.HUnit
import Data.Char (isAlpha, isDigit)
import Tokens

-- Test cases
tests :: Test
tests = TestList [
    "is_letter tests" ~: TestList [
        is_letter 'D' ~?= True,
        is_letter '<' ~?= False,
        is_letter ']' ~?= False
        -- add more test cases
    ],
    "is_digit tests" ~: TestList [
        is_digit 'c' ~?= False,
        is_digit '6' ~?= True
         -- add more test cases
    ],
    "alpha tests" ~: TestList [
        alpha ("", "varOne<varTwo") ~?= ("varOne", "<varTwo"),
        alpha ("roy", "8+myvar+(77+88)") ~?= ("roy", "8+myvar+(77+88)")
         -- add more test cases
    ],
    "numeric tests" ~: TestList [
        numeric ("", "45+kk") ~?= ("45", "+kk")
    ],
    "symbolic tests" ~: TestList [
        symbolic ("roy", "8<=myvar+(77+88)") ~?= ("roy", "8<=myvar+(77+88)"),
        symbolic ("roy", "<=myvar+(77+88)") ~?= ("roy", "<=myvar+(77+88)")
         -- add more test cases
    ],
    "scanning tests" ~: TestList [
        scanning ([], "45+kk") ~?= [Num "45", Key "+", Id "kk"],
        scanning ([Key "if", Key "true", Key "then"], "x:=1 else x:=2") ~?= [Key "if", Key "true", Key "then", Id "x", Key ":=", Num "1", Key "else", Id "x", Key ":=", Num "2"]
         -- add more test cases
    ],
    "tokenize tests" ~: TestList [
        tokenize "if true then x:=1 else y:=2" ~?= [Key "if", Key "true", Key "then", Id "x", Key ":=", Num "1", Key "else", Id "y", Key ":=", Num "2"],
        tokenize "myvar+(77+88)" ~?= [Id "myvar", Key "+", Key "(", Num "77", Key "+", Num "88", Key ")"]
         -- add more test cases
    ]
  ]

testTokens :: IO Counts
testTokens = runTestTT tests

