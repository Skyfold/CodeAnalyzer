{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Converter where

import Data.Text (isInfixOf, unpack)
import TextShow
import TextShow.Data.Char (showbLitString)
import Turtle hiding (toText)
import Prelude hiding (FilePath, concat)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

solveWithFOASL :: TextShow a => Formulae (Expr a) -> Shell Bool
solveWithFOASL input = do 
    currentDir <- pwd
    file <- using $ mktempfile currentDir "test"
    output file $ return $ toText $ showb input
    (exitCode, text) <- liftIO $ procStrict "lssl" [(format fp) file] empty
    return $ isInfixOf "Proof found" text

data Formulae a = Lit Bool
              | Emp
              | a :|-> [a]
              | a :== a 
              | a :> a
              | a :< a
              | a :<= a
              | a :>= a
              | Not (Formulae a) 
              | Formulae a :& Formulae a 
              | Formulae a :| Formulae a 
              | Formulae a :-> Formulae a 
              | Star (Formulae a) (Formulae a) 
              | Formulae a :-* Formulae a 
              | Exists a (Formulae a) 
              | Forall a (Formulae a) 
              | Ls a a 
              | Tr a
    deriving (Show, Functor, Traversable, Foldable)

data Expr a = Var a
          | Expr a :+ Expr a
          | Expr a :- Expr a
          | Expr a :* Expr a
          | Expr a :/ Expr a
          | Expr a :^ Expr a
    deriving (Show, Functor, Traversable, Foldable)

 -- unpack . showbLitString 

instance TextShow a => TextShow (Expr a) where
  showb (Var a) = showb a
  showb (a :+ b) = 
    showbParen True $ showb a <> " +" <> showbSpace <> showb b
  showb (a :- b) = 
    showbParen True $ showb a <> showbSpace <> "-" <> showbSpace <> showb b
  showb (a :* b) = 
    showbParen True $ showb a <> showbSpace <> "*" <> showbSpace <> showb b
  showb (a :/ b) = 
    showbParen True $ showb a <> showbSpace <> "/" <> showbSpace <> showb b
  showb (a :^ b) = 
    showbParen True $ showb a <> showbSpace <> "^" <> showbSpace <> showb b

instance TextShow a => TextShow (Formulae a) where
  showb (Lit True) = "true"
  showb (Lit False) = "false"
  showb Emp = "emp"
  showb (a :|-> b) = showbParen True $ showb a <> " |-> " <> listOfVar b
  showb (a :== b) = showbParen True $ showb a <> " = " <> showb b
  showb (a :> b) = showbParen True $ showb a <> " > " <> showb b
  showb (a :< b) = showbParen True $ showb a <> " < " <> showb b
  showb (a :>= b) = showbParen True $ showb a <> " >= " <> showb b
  showb (a :<= b) = showbParen True $ showb a <> " <= " <> showb b
  showb (Not f) = ("~" :: Builder) <> showbParen True (showb f)
  showb (a :& b) = showbParen True $ showb a <> " & " <> showb b
  showb (a :-> b) = showbParen True $ showb a <> " -> " <> showb b
  showb (Star a b) = showbParen True $ showb a <> " * "<> showb b
  showb (a :-* b) = showbParen True $ showb a <> " -* " <> showb b
  showb (Exists v f) =
    ("exists " :: Builder) <> showb v <> showbSpace <> showbParen True (showb f)
  showb (Forall v f) =
    ("forall " :: Builder) <> showb v <> showbSpace <> showbParen True (showb f)
  showb (Ls a b) = 
    ("ls" :: Builder) <> showbParen True (showb a <> ", " <> showb b)
  showb (Tr a) = 
    ("tr" :: Builder) <> showbParen True (showb a)

listOfVar :: TextShow a => [a] -> Builder
listOfVar c = case c of
  [] -> ""
  [x] -> showb x
  x:xs -> showb x <> "," <> listOfVar xs
