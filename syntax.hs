--- (c) Martin Braun, November 2015
--- 101lang is an Experimental language
--- used to learn the basics of programming
--- language design with Haskell
---
--- License: Apache 2.0
---
--- file: syntax.hs
module Syntax where

import Prelude

--- for now just ints
data Value = NumVal Int
    | StrVal String

instance Show Value where
    show (NumVal a) = show a
    show (StrVal a) = a

class Show a => Ops a where
    minus :: a -> a -> a
    plus :: a -> a -> a
    divide :: a -> a -> a
    times :: a -> a -> a
    greater :: a -> a -> Bool
    smaller :: a -> a -> Bool
    smaller a b = greater b a
    equal :: a -> a -> Bool
    equal a b = smaller a b == greater a b
    
instance Ops Value where
    minus (NumVal a) (NumVal b) = NumVal (a - b)
    plus (NumVal a) (NumVal b) = NumVal (a + b)
    plus (StrVal a) (StrVal b) = StrVal (a ++ b)
    plus (StrVal a) (NumVal b) = StrVal (a ++ (show b))
    plus (NumVal a) (StrVal b) = StrVal ((show a) ++ b)
    divide (NumVal a) (NumVal b) = NumVal (div a b)
    times (NumVal a) (NumVal b) = NumVal (a * b)
    greater (NumVal a) (NumVal b) = a > b

--- AST for Expressions
data Exp = Constant Value 
        | Variable String
        | Minus Exp Exp
        | Plus Exp Exp
        | Div Exp Exp
        | Times Exp Exp
        | Greater Exp Exp
        | Smaller Exp Exp
        | Equal Exp Exp
        | ReadString
        | ReadNum
        deriving Show

--- AST for statements
--- a sequence of statements longer than 2
--- is built using the Seq constructor
--- multiple times        
data Stmt = Assign String Exp
        | Seq Stmt Stmt
        | Cond Exp Stmt Stmt
        | While Exp Stmt
        | Declare String Exp Stmt
        | Print Exp
        deriving Show