--- (c) Martin Braun, November 2015
--- 101lang is an Experimental language
--- used to learn the basics of programming
--- language design with Haskell
---
--- License: Apache 2.0
---
--- file: interpreter.hs
module Interpreter where

import Syntax
import Prelude
import Control.Applicative
import Control.Monad
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
            
--- Our Monad used for evaluation
newtype Op m a = Op { runOp :: Stack -> m (a, Stack, String) }

instance Monad m => Functor (Op m) where
    fmap f o = Op $ \stack -> fmap (\(x,stck,str) -> (f x, stck, str)) (runOp o stack)

instance Monad m => Applicative (Op m) where
    pure x = Op $ \stack -> pure (x, stack, "")
    Op f <*> Op x = Op $ \stack -> do (y, stack', str1) <- f stack
                                      (a, stack'', str2) <- x stack'
                                      return (y a, stack'', str1 ++ str2)
    
instance Monad m => Monad (Op m) where
    return a = Op $ \stack -> return (a, stack, "")
    e >>= f = Op $ \stack -> do (val1, stack1, str1) <- runOp e stack
                                (val2, stack2, str2) <- runOp (f val1) stack1
                                return (val2, stack2, str1 ++ str2)

            
push :: String -> Value -> State -> State
push name value state = toState (value : (stack state), name : (index state), output state)

pop :: State -> State
pop state = toState (tail (stack state), tail (index state), output state)

--- pure implementation of the evaluation
eval                            :: Exp -> State -> Value
eval (Constant n) state         = n
eval (Variable x) state         = findVal x state
eval (Minus x y) state          = binary minus state x y
eval (Plus x y) state           = binary plus state x y
eval (Greater x y) state        = binaryBool greater state x y
eval (Smaller x y) state        = binaryBool smaller state x y
eval (Equal x y) state          = binaryBool equal state x y
eval (Times x y) state          = binary times state x y
eval (Div x y) state            = binary divide state x y

binary :: (Value -> Value -> Value) -> State -> Exp -> Exp -> Value
binary f state x y = f xVal yVal
        where xVal = eval x state
              yVal = eval y state
              
binaryBool :: (Value -> Value -> Bool) -> State -> Exp -> Exp -> Value
binaryBool f state x y = if (f xVal yVal) then (NumVal 1) else (NumVal 0)
        where xVal = eval x state
              yVal = eval y state
              
--- pure implementation of the command interpreter
interpret                           :: Stmt -> State -> State
interpret (Assign name e) state     = setVal name (eval e state) state
interpret (Seq s1 s2)     state     = interpret s2 stateAfter1
        where stateAfter1 = interpret s1 state
interpret (Cond e s1 s2) state      = go (eval e state) s1 s2 state
        where
            go (NumVal 1) s1 _ state = interpret s1 state
            go _ _ s2 state = interpret s2 state
interpret (While e b) state         = go (eval e state) e b state
        where
            go (NumVal 1) e b state = interpret (While e b) stateAfterB
                where stateAfterB = interpret b state
            go _ _ _ state = state
interpret (Declare name e rest) state = pop stateAfterRest
        where stateAfterRest = interpret rest stateAfterPush
                where stateAfterPush = push name (eval e state) state
interpret (Print e) state            = appendOutput state (show (eval e state))