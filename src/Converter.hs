{-# LANGUAGE OverloadedStrings #-}

module Converter (
    Formulae(..)
  , Expr(..)
  , Var
  , exprToText
  , genText
) where

import Data.Text
import Prelude hiding (concat)

data Formulae = Lit Bool
             | Mtrue 
             | Expr :|-> [Expr]
             | Expr :== Expr 
             | Not Formulae 
             | Formulae :& Formulae 
             | Formulae :-> Formulae 
             | Formulae :* Formulae 
             | Formulae :-* Formulae 
             | Exists Var Formulae 
             | Forall Var Formulae 
             | Ls Expr Expr 
             | Tr Expr

data Expr = Nil | Var Text

type Var = Text

instance Show Expr where
  show = unpack . exprToText

instance Show Formulae where
  show = unpack . genText

exprToText :: Expr -> Text
exprToText Nil = "nil"
exprToText (Var a) = a

genText :: Formulae -> Text
genText (Lit True) = "true" 
genText (Lit False) = "false" 
genText Mtrue = "mtrue" 
genText (a :|-> b) = concat ["(",exprToText a," |-> ",list b,")"]
genText (a :== b) = concat ["(",exprToText a," = ",exprToText b,")"]
genText (Not f) = concat ["~(",genText f,")"]
genText (a :& b) = concat ["(",genText a," & ",genText b,")"]
genText (a :-> b) = concat ["(",genText a," -> ",genText b,")"]
genText (a :* b) = concat ["(",genText a," * ",genText b,")"]
genText (a :-* b) = concat ["(",genText a," -* ",genText b,")"]
genText (Exists v f) = concat ["exists ",v,".(",genText f,")"]
genText (Forall v f) = concat ["forall ",v,".(",genText f,")"]
genText (Ls a b) = concat ["ls(",exprToText a,", ",exprToText b,")"]
genText (Tr a) = concat ["tr(",exprToText a,")"]

list :: [Expr] -> Text
list c = case c of
  [] -> ""
  [x] -> exprToText x
  x:xs -> concat [exprToText x, ",", list xs]
