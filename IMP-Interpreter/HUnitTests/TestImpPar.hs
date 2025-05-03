import Test.HUnit

import AST
import Tokens
import Parse 
import ImpPar


-- Test cases for makeBool
testMakeBool :: Test
testMakeBool = TestList [
    "Test 5 <= 2" ~: makeBool (Int 5, ("<=", Int 2)) ~?= BopExp (LeEq, Int 5, Int 2)
    --- add test cases
    ]

-- Test cases for makeSeq
testMakeSeq :: Test
testMakeSeq = TestList [
    "Test sequence of commands" ~: makeSeq (c1, lc) ~?= Seq ((Seq (c1, c2)),  c3)
    --- add test cases
    ]
  where
    c1 = If (Bool True, Ass ("x", Int 1), Ass ("x", Int 2))
    c2 = If (Bool False, Ass ("x", Int 3), Ass ("x", Int 4))
    c3 = If (Bool True, Ass ("x", Int 5), Ass ("x", Int 6))
    lc = [(";", c2), (";", c3)]

-- Test cases for makeRepeat
testMakeRepeat :: Test
testMakeRepeat = TestList [
    "Test repeat statement" ~: makeRepeat ("repeat", (c, ("until", b) )) ~?= 
        Repeat (Ass ("x", IopExp (Plus, Var "x", Int 1)), BopExp (Gr, Var "x", Int 0))
    ]
      where
    c = Ass ("x", IopExp (Plus, Var "x", Int 1))
    b = BopExp (Gr, Var "x", Int 0)
    
-- all tests for make functions
testMakes =TestList [testMakeBool, testMakeSeq, testMakeRepeat]


------------------------------------------
---  Testing the combinators
----------------------------------------

-- Test cases for iexp
iexpTests :: Test
iexpTests = TestList [
    "iexp 8*2+(4*3)" ~: iexp tokens1                 
        ~?= Right (IopExp (Plus, IopExp (Times, Int 8, Int 2), IopExp (Times, Int 4, Int 3)), []),
    "iexp 1+2*3" ~: iexp  tokens2
        ~?= Right (IopExp (Plus, Int 1, IopExp (Times, Int 2, Int 3)), []),
    "iexp 1*2+3" ~: iexp tokens3
        ~?= Right (IopExp (Plus, IopExp (Times, Int 1, Int 2), Int 3), [])
        --- add test cases
  ]
              where   tokens1 = [Num "8",Key "*",Num "2",Key "+",Key "(",Num "4",Key "*",Num "3",Key ")"]
                       --- (tokenize "8*2+(4*3)") 
                      tokens2 = [Num "1",Key "+",Num "2",Key "*",Num "3"]
                      -- (tokenize "1+2*3")
                      tokens3 = [Num "1",Key "*",Num "2",Key "+",Num "3"]
                      ---(tokenize "1*2+3") 

-- Test cases for readiexp
readiexpTests :: Test
readiexpTests = TestList [
    "readiexp 8+1*2" ~: readiexp "8+1*2" 
        ~?= Right (IopExp (Plus, Int 8, IopExp (Times, Int 1, Int 2)))
  ]

-- Test cases for factor
factorTests :: Test
factorTests = TestList [
    "factor 8*2" ~: factor (tokenize "8*2") 
        ~?= Right (IopExp (Times, Int 8, Int 2), []),
    "factor 8*2*9" ~: factor (tokenize "8*2*9") 
        ~?= Right (IopExp (Times, IopExp (Times, Int 8, Int 2), Int 9), [])
        --- add test cases
  ]


-- all tests for expressions

testExp = TestList [iexpTests, readiexpTests, factorTests]

------------------------------
-- Testing combinators for commands
--------------------------------------

-- Test cases for `catom`
testCatom :: Test
testCatom = TestList
  [ "Parse simple assignment" ~: 
      reader catom "x:= 1" ~?= Right (Ass ("x", Int 1))
  , "Parse while loop" ~:
      reader catom "(x:=1; while y<x do x:= x+1)" ~?= 
        Right (Seq (Ass ("x", Int 1), While (BopExp (Le, Var "y", Var "x"), Ass ("x", IopExp (Plus, Var "x", Int 1)))))
        --- add test cases
  ]

-- Test cases for `iwr`
testIwr :: Test
testIwr = TestList
  [ "Parse if statement" ~:
      reader iwr "if true then x:=4 else x:=5" ~?= 
        Right (If (Bool True, Ass ("x", Int 4), Ass ("x", Int 5)))
  , "Parse while loop" ~:
      reader iwr "while true do y:=7" ~?= Right (While (Bool True, Ass ("y", Int 7)))
  , "Parse invalid syntax" ~:
      reader iwr "while true do y:=7; x:=1" ~?= Left SyntaxError
      --- add test cases
  ]

-- Test cases for `com`
testCom :: Test
testCom = TestList
  [ "Parse if-else with while" ~:
      readcom "if true then a:=4 else (while false do a:=4)" ~?= 
        Right (If (Bool True, Ass ("a", Int 4), While (Bool False, Ass ("a", Int 4))))
  , "Parse nested sequences" ~:
      readcom "while true do (((x:=1) ; (y:=2)) ; (z:=3))" ~?= 
        Right (While (Bool True, Seq (Seq (Ass ("x", Int 1), Ass ("y", Int 2)), Ass ("z", Int 3))))
        --- add test cases
  ]

-- Test cases for `reader` with `next` and `build`
testReaderNextBuild :: Test
testReaderNextBuild = TestList
  [ "Without make function" ~:
      (idr `next` key":=" `next` iexp) [Id "var", Key ":=", Num "8"] ~?= Right (("var", (":=", Int 8)), [])
  , "With make function" ~:
      (idr `next` key":=" `next` iexp `build` makeAss) [Id "var", Key ":=", Num "8"] ~?= Right (Ass ("var", Int 8), [])
  ]

-- all tests for commands
testCommands = TestList [testCatom, testIwr, testCom, testReaderNextBuild]

-------------------------------------
--- Run all tests for ImpPar 
------------------------------------
testImpPar :: IO Counts
testImpPar = runTestTT $ TestList [ testMakes, testExp, testCommands]

