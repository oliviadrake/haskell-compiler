module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N int) = [LOADI int]
acomp (V var) = [LOAD var]
acomp (Plus x y) = acomp x ++ acomp y ++ [ADD]

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc bool) jump number = [JMP number | bool == jump]
bcomp (Not bool) jump number = bcomp bool (not jump) number
bcomp (And x y) jump number =
    let compiledy = bcomp y jump number;
        compiledx = if jump then bcomp x False (length compiledy) else bcomp x False (length compiledy + number);
    in compiledx ++ compiledy
bcomp (Less x y) jump number = acomp x ++ acomp y ++ (if jump then [JMPLESS number] else [JMPGE number])

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp (Assign vname aexp) = acomp aexp ++ [STORE vname]
ccomp (Seq c1 c2) = ccomp c1 ++ ccomp c2
ccomp (If bool c1 c2) =
    let compiledc1 = ccomp c1;
        compiledc2 = ccomp c2;
        compiledbool = bcomp bool False (length compiledc1 + 1)
        jumper = [JMP (length compiledc2) | compiledbool /= []]
    in compiledbool ++ compiledc1 ++ jumper ++ compiledc2
ccomp (While bool c) =
    let compiledc = ccomp c;
        compiledbool = bcomp bool False (length compiledc + 1)
    in compiledbool ++ compiledc ++ [JMP (-1 * (length compiledc + length compiledbool +1))]
ccomp SKIP = []