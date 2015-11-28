--- (c) Martin Braun, November 2015
--- 101lang is an experimental language
--- used to learn the basics of programming
--- language design with Haskell
---
--- License: Apache 2.0
---
--- file: syntax.hs

module Syntax where

import Prelude

--- for now just ints
type Value = Int

--- AST for Expressions
data Exp = Constant Value
        | Variable String
        | Minus Exp Exp
        | Greater Exp Exp
        | Times Exp Exp
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




