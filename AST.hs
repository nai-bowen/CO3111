  
--------------------------------------------------------------------
-- HASKELL ABSTRACT SYNTAX TREES FOR IMP                  
-- Roy Crole and Paula Severi 2025                                           
--------------------------------------------------------------------

module AST where 

type V = String
type B = Bool
type Z = Int
type FileName = String


data Error =  SyntaxError | UninitializedVar
              deriving (Eq, Show)   
              

-- COMPLETE THE TYPE FOR THE STATE 
--type State = ???


data Instruction = Run FileName | Eval Prog | Quit 
                   deriving (Eq,Show)

type Prog  = (Code,State)  


data Code  = E IntExp | C Com
             deriving (Eq,Show)


data Bop =   Le | Gr | LeEq | GrEq
             deriving (Eq,Show)

data Iop = Plus | Minus | Times 
             deriving (Eq,Show)

-- COMPLETE THE FOLLOWING DATA TYPES

--data Com   =  ???
             
--data BoolExp   =  ???
             
--data IntExp   =  ???       
             




              




