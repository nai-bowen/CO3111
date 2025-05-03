import Test.HUnit
import System.Directory
import Files (processLine, processLines, processAll, processFile)

-- Example input data
line1 :: String
line1 = "(E 5+7,[])"

line2 :: String
line2 = "(C x:= 1; y:= 2; z:= (x +y), [])"

line3 :: String
line3 = "(C if x>0 then sign:= 1 else sign := -1, [(x, -3)])"

exampleLines :: [String]
exampleLines = [line1, line2, line3]

exampleString :: String
exampleString = " (E 5+7,[]) \n (C x:=1; y:=2; z:= (x+y), [])"

-- Unit tests
testProcessLine :: Test
testProcessLine = TestList
  [ "test line1" ~: processLine line1 ~?= "(E 5+7,[]) ==> 12"
  , "test line2" ~: processLine line2 ~?= "(C x:= 1; y:= 2; z:= (x +y), []) ==> [(\"x\",1),(\"y\",2),(\"z\",3)]"
  , "test line3" ~: processLine line3 ~?= "(C if x>0 then sign:= 1 else sign := -1, [(x, -3)]) ==> [(\"x\",-3),(\"sign\",-1)]"
  ]

testProcessLines :: Test
testProcessLines = "test processLines" ~:
  processLines exampleLines ~?=
    [ "(E 5+7,[]) ==> 12"
    , "(C x:= 1; y:= 2; z:= (x +y), []) ==> [(\"x\",1),(\"y\",2),(\"z\",3)]"
    , "(C if x>0 then sign:= 1 else sign := -1, [(x, -3)]) ==> [(\"x\",-3),(\"sign\",-1)]"
    ]

testProcessAll :: Test
testProcessAll = "test processAll" ~:
  processAll exampleString ~?=
    " (E 5+7,[])  ==> 12\n (C x:=1; y:=2; z:= (x+y), []) ==> [(\"x\",1),(\"y\",2),(\"z\",3)]\n"


-- Since processFile involves I/O, we need to test it using temporary files.
-- Make sure you have downloaded the following files:
--     fileA, fileB, fileC, fileD, and fileE,
--     along with all files that have the ".ExpectedOutput" extension.
-- testProcessFile is a single function; we do not write a separate code for each file (Code reuse).


testProcessFile :: String -> Test
testProcessFile  x = TestCase $ do
  putStrLn ("\nYour file is "++ x)
  existsFile <- doesFileExist x
  existsExpected <- doesFileExist (x++ ".ExpectedOutput")
  if (not existsFile) then
     assertFailure ("Input file "++ x ++ " does not exist")
  else if (not existsExpected) then
       assertFailure ("Expect output file for input "++ x ++ " does not exist")
       else  do
          processFile x 
          putStrLn ("The file " ++ x ++ " was processed ")
          result <- readFile (x++ ".Output")
          expected <- readFile (x++ ".ExpectedOutput")
          assertEqual ("test processFile "++ x) expected result
  

testFunFiles :: IO Counts
testFunFiles = runTestTT $ TestList [testProcessLine, testProcessLines, testProcessAll]

testProcessManyFiles :: IO Counts
testProcessManyFiles = runTestTT $ TestList [
                                  testProcessFile "fileA",
                                  testProcessFile "fileB",
                                  testProcessFile "fileC", 
                                  testProcessFile "fileD", 
                                  testProcessFile "fileE"  
                                  ]

