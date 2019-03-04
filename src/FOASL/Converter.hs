{-# LANGUAGE OverloadedStrings #-}

module FOASL.Converter (solveWithFOASL) where

import Data.Text (isInfixOf, Text)
import Turtle ( pwd, mktempfile, using, procStrict
              , format, fp, empty, Shell, output, unsafeTextToLine)
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (FilePath)
import FirstOrderLogic.Syntax (Formulae (..))
import TextShow (showb, showbParen, Builder, TextShow, toText)
import Data.Monoid ((<>))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Turtle

-- | 'solveWithFOASL' asks the executable @lssl@ if it can generate a proof for the given 'Formulae'. It only takes variables represented as Text. Variables are alpha-numeric. FOASL cannot be sent variables with @x@ in them, so internally I keep a mapping of expressions to variables of the form @y(integer)@.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> view (solveWithFOASL (Not ((Star (Not Emp) (Not Emp)) :-* (Lit False))))
-- True
--
-- >>> view (solveWithFOASL (("y1" :> "y2") :-> (Not ("y1" :== "y2"))) )
-- False
--
-- >>> view (solveWithFOASL ((("y" :== "y1") :& ("y1" :|-> ["y2"])) :-> ((("y" :== "y1") :& ("y1" :|-> ["y2"])) :& ("y" :|-> ["y2"]))) )
-- True
--
solveWithFOASL :: Formulae Text -> Shell Bool
solveWithFOASL input = do 
    currentDir <- pwd
    file <- using $ mktempfile currentDir "test"
    output file $ return $ unsafeTextToLine $ toText $ formatForFOASL input
    (_, text) <- liftIO $ procStrict "lssl" [(format fp) file] empty
    return $ isInfixOf "Proof found" text

formatForFOASL :: Formulae Text -> Builder
formatForFOASL formulae = case formulae of 
    (Lit True) -> "true"
    (Lit False) -> "false"
    Emp -> "emp"
    (a :|-> b) -> showbParen True $ showb a <> " |-> " <> listOfVar b
    (a :== b) -> showbParen True $ showb a <> " = " <> showb b
    (a :/= b) -> "(~" <> "(" <> showb a <> " = " <> showb b <> "))"
    (a :> b) -> showbParen True $ showb a <> " > " <> showb b
    (a :< b) -> showbParen True $ showb a <> " < " <> showb b
    (a :>= b) -> showbParen True $ showb a <> " >= " <> showb b
    (a :<= b) -> showbParen True $ showb a <> " <= " <> showb b
    (Not f) -> ("~" :: Builder) <> showbParen True (formatForFOASL f)
    (a :& b) -> showbParen True $ formatForFOASL a <> " & " <> formatForFOASL b
    (a :| b) -> showbParen True $ formatForFOASL a <> " | " <> formatForFOASL b
    (a :-> b) -> showbParen True $ formatForFOASL a <> " -> " <> formatForFOASL b
    (a :~& b) -> ("~(" :: Builder) <> formatForFOASL a <> " & " <> formatForFOASL b <> ")"
    (a :~| b) -> ("~(" :: Builder) <> formatForFOASL a <> " | " <> formatForFOASL b <> ")"
    (a :<+> b) -> "(" <> formatForFOASL a <> " & (~(" <>  formatForFOASL b <> ")))"
        <> " | ((~(" <> formatForFOASL a <> ")) & " <> formatForFOASL b <> ")"
    (a :<=> b) -> "(" <> formatForFOASL a <> " & " <>  formatForFOASL b <> ")"
        <> " | ((~(" <> formatForFOASL a <> ")) & (~(" <> formatForFOASL b <> ")))"
    (Star a b) -> showbParen True $ formatForFOASL a <> " * "<> formatForFOASL b
    (a :-* b) -> showbParen True $ formatForFOASL a <> " -* " <> formatForFOASL b

listOfVar :: TextShow a => [a] -> Builder
listOfVar c = case c of
  [] -> ""
  [x] -> showb x
  x:xs -> showb x <> "," <> listOfVar xs
