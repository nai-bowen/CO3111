--------------------------------------------------------------------
-- HASKELL ABSTRACT SYNTAX TREES FOR IMP                  
-- Roy Crole and Paula Severi 2025                                           
--------------------------------------------------------------------

module AST where 

type V = String
type B = Bool
type Z = Int
type FileName = String

data Error = SyntaxError | UninitializedVar
              deriving (Eq, Show)   
              
-- State represents memory as list of variable-value pairs
type State = [(V, Z)]

data Instruction = Run FileName | Eval Prog | Quit 
                   deriving (Eq,Show)

type Prog = (Code,State)  

data Code = E IntExp | C Com
             deriving (Eq,Show)

data Bop = Le | Gr | LeEq | GrEq
             deriving (Eq,Show)

data Iop = Plus | Minus | Times 
             deriving (Eq,Show)

-- Commands in IMP
data Com = Ass (V, IntExp)                      -- Assignment: x := e
         | Seq (Com, Com)                       -- Sequence: c1 ; c2
         | If (BoolExp, Com, Com)               -- Conditional: if b then c1 else c2
         | While (BoolExp, Com)                 -- While loop: while b do c
         | Repeat (Com, BoolExp)                -- Repeat-until: repeat c until b
         deriving (Eq, Show)
             
-- Boolean expressions
data BoolExp = Bool B                           -- Boolean constant: true, false
             | BopExp (Bop, IntExp, IntExp)     -- Comparison: e1 < e2, e1 <= e2, etc.
             deriving (Eq, Show)
             
-- Integer expressions
data IntExp = Int Z                             -- Integer constant: 5, -3, etc.
            | Var V                             -- Variable: x, y, etc.
            | IopExp (Iop, IntExp, IntExp)      -- Operation: e1 + e2, e1 - e2, etc.
            deriving (Eq, Show)