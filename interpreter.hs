--- (c) Martin Braun, November 2015
--- 101lang is an experimental language
--- used to learn the basics of programming
--- language design with Haskell
---
--- License: Apache 2.0
---
--- file: interpreter.hs

module Interpreter where

import Syntax
import Prelude
import Debug.Trace (trace)

type Location = Int
type Index = [String]
type Stack = [Value]

data State = State { stack :: Stack,
               index :: Index,
               output :: String }
               
instance Show State where
    show state = output state

setStack :: State -> Stack -> State
setStack state stack = toState (stack, index state, output state)

appendOutput :: State -> String -> State
appendOutput state string = toState (stack state, index state, (output state) ++ string)

toState :: (Stack, Index, String) -> State
toState (stack, index, output) =
        State { stack = stack, index = index, output = output }
        

varNotFound = error "could not find some variable on the stack"

--- find the position of the given variable name
--- in the given index
position            :: String -> Index -> Int
position name index = go 0 index name
        where 
            go _ [] _ = error ("undefined variable " ++ name)
            go curPos (x:xs) name
                | x == name = curPos
                | otherwise = go (curPos + 1) xs name
                
fetch               :: Location -> Stack -> Value
fetch _ []          = varNotFound
fetch 0 (x:xs)      = x
fetch n (x:xs)      = fetch (n - 1) xs

put                 :: Location -> Value -> Stack -> Stack
put _ _ []          = varNotFound
put 0 x (v:vs)      = x:vs
put n x (v:vs)      = v:(put (n-1) x vs)

--- utility functions
findVal             :: String -> State -> Value
findVal name state  = fetch pos (stack state)
        where
            pos = position name (index state)
            
setVal              :: String -> Value -> State -> State
setVal name value state = setStack state (put pos value (stack state))
        where
            pos = position name (index state)
            
push :: String -> Value -> State -> State
push name value state = toState (value : (stack state), name : (index state), output state)

pop :: State -> State
pop state = toState (tail (stack state), tail (index state), output state)

--- pure implementation of the evaluation
eval                    :: Exp -> State -> Value
eval (Constant n) state = n
eval (Variable x) state = findVal x state
eval (Minus x y) state  = xVal - yVal
        where xVal = eval x state
              yVal = eval y state
eval (Greater x y) state = if xVal > yVal then 1 else 0
        where xVal = eval x state
              yVal = eval y state
eval (Times x y) state = xVal * yVal
        where xVal = eval x state
              yVal = eval y state
              
--- pure implementation of the command interpreter
interpret                           :: Stmt -> State -> State
interpret (Assign name e) state     = setVal name (eval e state) state
interpret (Seq s1 s2)     state     = interpret s2 stateAfter1
        where stateAfter1 = interpret s1 state
interpret (Cond e s1 s2) state      = go (eval e state) s1 s2 state
        where
            go 1 s1 _ state = interpret s1 state
            go 0 _ s2 state = interpret s2 state
interpret (While e b) state         = go (eval e state) e b state
        where
            go 1 e b state = interpret (While e b) stateAfterB
                where stateAfterB = interpret b state
            go 0 _ _ state = state
interpret (Declare name e rest) state = pop stateAfterRest
        where stateAfterRest = interpret rest stateAfterPush
                where stateAfterPush = push name (eval e state) state
interpret (Print e) state            = appendOutput state (show (eval e state))