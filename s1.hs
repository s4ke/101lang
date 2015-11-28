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

s1 = Declare "x" (Constant 150)
        (Declare "y" (Constant 200)
            (Seq (While (Greater (Variable "x" ) (Constant 0)
                        )
                        (Seq (Assign "x" (Minus (Variable "x")
                                                (Constant 1)
                                          )
                              )
                              (Assign "y" (Minus (Variable "y")
                                                 (Constant 1)
                                           )
                              )
                        )
                  )
                  (Print (Variable "y"))
            )
        )
        
main = do       
    putStrLn (show (interpret s1 State { stack = [], index = [], output = "" }))