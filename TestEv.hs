module TestBasic where

import Test.HUnit
import AST
import Basic
import EvSem


-- Some abbreviations that can be used in the test cases
---  add more abbreviations for expressions or commands to use in your test cases

exp1 = (IopExp(Plus, IopExp(Minus,Int 4,Var "n") , Int 6))  -- "4-n+6"
exp2 = (IopExp(Plus, IopExp(Times,Var "m",exp1) , Int 6))   -- "m*(4-n+6)+6"

bexp0 = BopExp (Le,IopExp (Plus,Var "a",Var "b"),Int 4)  --- a+ b < 4
exp3 = IopExp (Plus,Var "a",Var "b")   -- a+b 
exp4 = IopExp (Times, Var "c", Int 1)     -- c*1
bexp3 = BopExp (Le, exp3, exp4)     --- a+b < c*1
 
ass = Ass ("x", Int 32)   --  x:= 32 
seq1 = Seq (Ass ("x",Int 34),Ass ("y",Int 8))  -- "x:=34 ; y:=8"
maxAST = If (BopExp (Gr,Var "x",Var "y"),Ass ("max",Var "x"),Ass ("max",Var "y"))
mywhile  = While (BopExp (Gr,Var "a",Int 0),Ass ("a",IopExp (Minus,Var "a",Int 1)))
myrepeat = Repeat (Ass ("X",IopExp (Times,Var "X",Int 2)),BopExp (Gr,Var "X",Var "N"))




tests :: Test
tests = TestList [
    -- Tests for evalint
    "test_evalint_uninitialized" ~: (evalint exp1 []) ~?= Left UninitializedVar,
    "test_evalint_valid" ~: evalint exp1 [("n", 10)] ~?= Right 0,
      --- add test cases 
   
      -- Tests for evalbool
    "test_evalbool_true" ~: evalbool bexp0 [("a", 1), ("b", 2)] ~?= Right True,
    ---  add test cases
    
    -- Tests for evalcom
    "test_evalcom_assignment" ~: evalcom ass [] ~?= Right [("x", 32)]
    --- add test cases 
  ]
  
  
runEvTests :: IO Counts
runEvTests = runTestTT tests

