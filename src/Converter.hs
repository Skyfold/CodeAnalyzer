{-# LANGUAGE OverloadedStrings #-}

module Converter where

import Data.Text hiding (empty)
import Turtle
import Prelude hiding (FilePath, concat)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Filesystem.Path.CurrentOS (toText)

solveWithFOASL :: Text -> Shell Bool
solveWithFOASL input = do 
    currentDir <- pwd
    file <- using $ mktempfile currentDir "test"
    output file (return input)
    let fileStr = either (error "handle this in solveFOASL") (id) $ toText file
    (exitCode, text) <- liftIO $ procStrict "./lssl" [fileStr] empty
    return $ isInfixOf "Proof found" text

data Formulae = Lit Bool
              | Emp
              | Expr :|-> [Expr]
              | Expr :=== Expr 
              | Expr :> Expr
              | Expr :< Expr
              | Expr :<= Expr
              | Expr :>= Expr
              | Not Formulae 
              | Formulae :& Formulae 
              | Formulae :| Formulae 
              | Formulae :-> Formulae 
              | Star Formulae Formulae 
              | Formulae :-* Formulae 
              | Exists Var Formulae 
              | Forall Var Formulae 
              | Ls Expr Expr 
              | Tr Expr

data Expr = Var Text
          | Expr :+ Expr
          | Expr :- Expr
          | Expr :* Expr
          | Expr :/ Expr
          | Expr :^ Expr

type Var = Text

instance Show Expr where
  show = unpack . exprToText

instance Show Formulae where
  show = unpack . genText

exprToText :: Expr -> Text
exprToText (Var a) = a
exprToText (a :+ b) = concat ["(",exprToText a," + ",exprToText b,")"]
exprToText (a :- b) = concat ["(",exprToText a," - ",exprToText b,")"]
exprToText (a :* b) = concat ["(",exprToText a," * ",exprToText b,")"]
exprToText (a :/ b) = concat ["(",exprToText a," / ",exprToText b,")"]
exprToText (a :^ b) = concat ["(",exprToText a," ^ ",exprToText b,")"]

genText :: Formulae -> Text
genText (Lit True)          = "true"
genText (Lit False)         = "false"
genText Emp                 = "emp"
genText (a :|-> b)          = concat ["(",exprToText a," |-> ",listOfVar b,")"]
genText (a :=== b)          = concat ["(",exprToText a," = ",exprToText b,")"]
genText (a :> b)            = concat ["(",exprToText a," > ",exprToText b,")"]
genText (a :< b)            = concat ["(",exprToText a," < ",exprToText b,")"]
genText (a :>= b)           = concat ["(",exprToText a," >= ",exprToText b,")"]
genText (a :<= b)           = concat ["(",exprToText a," <= ",exprToText b,")"]
genText (Not f)             = concat ["~(",genText f,")"]
genText (a :& b)            = concat ["(",genText a," & ",genText b,")"]
genText (a :-> b)           = concat ["(",genText a," -> ",genText b,")"]
genText (Star a b)            = concat ["(",genText a," * ",genText b,")"]
genText (a :-* b)           = concat ["(",genText a," -* ",genText b,")"]
genText (Exists v f)        = concat ["exists ",v,".(",genText f,")"]
genText (Forall v f)        = concat ["forall ",v,".(",genText f,")"]
genText (Ls a b)            = concat ["ls(",exprToText a,", ",exprToText b,")"]
genText (Tr a)              = concat ["tr(",exprToText a,")"]

listOfVar :: [Expr] -> Text
listOfVar c = case c of
  [] -> ""
  [x] -> exprToText x
  x:xs -> concat [exprToText x, ",", listOfVar xs]
