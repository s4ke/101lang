--- (c) Martin Braun, November 2015
--- 101lang is an experimental language
--- used to learn the basics of programming
--- language design with Haskell
---
--- License: Apache 2.0
---
--- file: s1.hs
module Main where

import Syntax
import Interpreter

s1 = Declare "x" (Constant (NumVal 150))
        (Declare "y" (Constant (NumVal 200))
            (Seq (While (Greater (Variable "x" ) (Constant (NumVal 0))
                        )
                        (Seq (Assign "x" (Minus (Variable "x")
                                                (Constant (NumVal 1))
                                          )
                              )
                              (Assign "y" (Minus (Variable "y")
                                                 (Constant (NumVal 1))
                                           )
                              )
                        )
                  )
                  (Print (Variable "y"))
            )
        )

s2 = Declare "x" (Constant (NumVal 100))
        (Seq (Assign "x" (Plus (Variable "x")
                               (Constant (NumVal 42))
                         )
              )
              (Print (Variable "x"))
         )
         
s3 = Print (Constant (StrVal "Hello World!"))

s4 = Print (Plus (Constant (StrVal "Test "))
                 (Constant (NumVal 1))
            )
        
main = do       
    putStrLn (show (interpret s1 State { stack = [], index = [], output = "" }))
    putStrLn (show (interpret s2 State { stack = [], index = [], output = "" }))
    putStrLn (show (interpret s3 State { stack = [], index = [], output = "" }))
    putStrLn (show (interpret s4 State { stack = [], index = [], output = "" }))