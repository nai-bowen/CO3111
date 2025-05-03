module TestParse where

import Test.HUnit

import AST
import Tokens
import Parse 

-- Function makePlus

makePlus (n , ("+", m)) = IopExp (Plus, Int (read n), Int (read m))


-- Test cases for key function
testKey :: Test
testKey = TestList
  [ "key + on '+8'" ~: (key "+") (tokenize "+8") ~?= Right ("+", [Num "8"])
  , "key - on '+8'" ~: (key "-") (tokenize "+8") ~?= Left SyntaxError
  , "key + on '8+8'" ~: (key "+") (tokenize "8+8") ~?= Left SyntaxError
  ]

-- Test cases for idr function
testIdr :: Test
testIdr = TestList
  [ "idr on 'myvar77+8'" ~: idr (tokenize "myvar77+8") ~?= Right ("myvar", [Num "77", Key "+", Num "8"])
  -- add more test cases 
  ]


-- Test cases for num function
testNum :: Test
testNum = TestList
  [ "num on '89'" ~: num (tokenize "89") ~?= Right ("89", [])
  , "num on 'var-77+8'" ~: num (tokenize "var-77+8") ~?= Left SyntaxError
  -- add more test cases 
  ]

-- Test cases for alt function
testAlt :: Test
testAlt = TestList
  [ "alt on '+8'" ~: (key "+" `alt` key "-") (tokenize "+8") ~?= Right ("+", [Num "8"])
  , "alt on '-8'" ~: (key "+" `alt` key "-") (tokenize "-8") ~?= Right ("-", [Num "8"])
  -- add more test cases 
  ]

-- Test cases for next function
testNext :: Test
testNext = TestList
   [ "next on '6+'"  ~: (num `next` key "+") [Num "6", Key "+"] ~?=  Right (("6", "+"), []) 
   , "next on 'r+'" ~: (num `next` key "+") [Id "r", Key "+"] ~?= Left SyntaxError
   -- add more test cases 
    ]

-- Test cases for many function
testMany :: Test
testMany = TestList
  [ "many num on '6 8 +'" ~: many num [Num "6", Num "8", Key "+"] ~?= Right (["6", "8"], [Key "+"])
  -- add more test cases 
  ]

-- Test cases for build function and makePlus
testBuild :: Test
testBuild = TestList
  [ "build makePlus on '6+8'" ~: (num `next` key "+" `next` num `build` makePlus) [Num "6", Key "+", Num "8"] ~?= Right (IopExp (Plus, Int 6, Int 8), [])
  ]

-- Run all tests
testParse :: IO Counts
testParse = runTestTT $ TestList [testKey, testIdr, testNum, testAlt, testNext, testMany, testBuild]

