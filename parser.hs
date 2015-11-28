--- (c) Martin Braun, November 2015
--- 101lang is an experimental language
--- used to learn the basics of programming
--- language design with Haskell
---
--- License: Apache 2.0
---
--- file: parser.hs
module Parser where

import Syntax

import Text.ParserCombinators.Parsec

{-
-- Parsing the expression
parseInteger = do sign <- option "" (string "-")
		  number <- many1 digit
		  return $ BlaiseInt (read (sign++number))

parseSymbol = do f <- firstAllowed
		 r <- many (firstAllowed <|> digit)
		 return $ BlaiseSymbol (f:r)
	where firstAllowed = oneOf "+-*/" <|> letter

parseExprAux = (try parseInteger) <|> (try parseSymbol) <|> (try parseList)

parseList = do char '(' ; skipMany space
	       x <- parseExprAux `sepEndBy` (many1 space)
	       char ')'
	       return $ BlaiseList x

parseExpr = do skipMany space
	       x <- parseExprAux
	       skipMany space ; eof
	       return x

parse :: String -> BlaiseResult
parse source = case (Text.ParserCombinators.Parsec.parse parseExpr "" source) of
		 Right x -> return x
		 Left e -> throwError $ show e
         
         -}