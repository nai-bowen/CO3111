module TestBasic where

import Test.HUnit
import AST
import Basic



-- Test cases
memTests = TestList [
    TestCase (assertEqual "mem 3 [1,2,3,4]" True (mem 3 [1,2,3,4]))
    --  add more test cases 
    --, TestCase (assertEqual string expectedvalue actual value)
  ]

lookUpTests = TestList [
   TestCase (assertEqual "lookUp [ (n, 2), (x, 10), (z, 100)] x"
                          (Right 10) 
                          (lookUp [("n", 2), ("x", 10), ("z", 100)] "x"))
     --  add more test cases 
    --, TestCase (assertEqual string expectedvalue actual value)
  ]

updateTests = TestList [
    TestCase (assertEqual "update [ (\"n\", 2), (\"x\", 10), (\"z\", 100)] \"x\" 34"
                          [("n",2),("x",34),("z",100)]
                          (update [("n", 2), ("x", 10), ("z", 100)] "x" 34))
   --  add more test cases 
    --, TestCase (assertEqual string expectedvalue actual value)
  ]

allBasicTests = TestList [memTests, lookUpTests, updateTests]

runTestBasic :: IO Counts
runTestBasic = runTestTT allBasicTests

