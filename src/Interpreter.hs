module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine

--TODO Task 2.1
data AExp =
    N Int | V Vname | Plus AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.2
aval :: AExp -> State -> Val
aval (N int) state = int
aval (V var) state = state ! var
aval (Plus x y) state = aval x state + aval y state

--TODO Task 2.1
data BExp =
    Bc Bool | Not BExp | And BExp BExp | Less AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool
bval (Bc bool) state = bool
bval (Not bool) state = not (bval bool state)
bval (And x y) state = bval x state && bval y state
bval (Less x y) state = aval x state < aval y state

--TODO Task 2.1
data Com =
    SKIP | Assign Vname AExp | Seq Com Com | If BExp Com Com | While BExp Com
    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State
eval (Assign vname aexp) state = insert vname (aval aexp state) state
eval (Seq c1 c2) state = eval c2 (eval c1 state)
eval (If bool c1 c2) state = if bval bool state then eval c1 state else eval c2 state
eval (While bool c1) state = if bval bool state then eval (While bool c1) (eval c1 state) else state
eval SKIP state = state