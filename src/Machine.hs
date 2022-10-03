{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Machine
(
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map

--TODO Task 1.1
type Vname = String
--TODO Task 1.2
type Val = Int
--TODO Task 1.3
type State = Map Vname Val

--TODO Task 1.4
data Instr =
        LOADI Val | LOAD Vname | ADD | STORE Vname | JMP Int | JMPLESS Int | JMPGE Int
        deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack = [Val]

--TODO Task 1.6
type Config = (Int, State, Stack)

--TODO Task 1.7
iexec :: Instr -> Config -> Config
iexec (LOADI x) (counter, state, stack) = (counter+1, state, x:stack)
iexec (LOAD var) (counter, state, stack) = (counter+1, state, (state ! var):stack)
iexec ADD (counter, state, x:y:stack) = (counter+1, state, (x+y):stack)
iexec (STORE var) (counter, state, x:stack) = (counter+1, insert var x state, stack)
iexec (JMP int) (counter, state, stack) = (counter+int+1, state, stack)
iexec (JMPLESS int) (counter, state, x:y:stack) = if y<x then (counter+int+1, state, stack) else (counter+1, state, stack)
iexec (JMPGE int) (counter, state, x:y:stack) = if y>=x then (counter+int+1, state, stack) else (counter+1, state, stack)

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec [] config = config
exec (x:xs) config = exec xs (iexec x config)