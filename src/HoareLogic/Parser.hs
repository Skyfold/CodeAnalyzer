
module HoareLogic.Parser  where

import Text.Trifecta
import HoareLogic.Structure
import Control.Lens (use, uses, (%=))
import Data.HashMap.Strict (empty, fromList, insert, lookup)
import Prelude hiding (lookup)
import FirstOrderLogic.Parser

-- | @proofSequent@ 
-- >>> parseString proofSequent mempty "// {-@ forall n, a (sum = n * a) @@ sum = (j * a) @-}\n sum := 0;\n j := 0;\nwhile (j /= n) {\nsum := sum + a;\nj := j + 1;}"
--
-- proofSequent :: (Monad m, TokenParsing m) => m ProofSequent
-- proofSequent = do
--     whiteSpace
--     (postCon, listOfInvariants) <- annotation
--     listOfSequents <- some sequent
--     finalVarTypes <- use mapOfVarType
--     return $ ProofSequent listOfSequents postCon listOfInvariants finalVarTypes

-- annotation :: (Monad m, TokenParsing m) => m (Condition, [Condition])
-- annotation = commentStart >> symbol "{-@" >> do
--     postCon <- some $ noneOf "@"
--     listOfInvariants <- many $ 
--         (symbol "@@" >> some (noneOf "@") <?> "Expecting loop invariant")
--     _ <- symbol "@-}"
--     return $ (postCon, listOfInvariants)

-- -- | @sequent@
-- -- >>> parseString (evalStateT sequent pseudocode) mempty "if (y < 6) {Int8 x := 8;}"
-- -- Success (IfThenElse "y < 6" [Assignment "x" "8"] [])
-- --
-- sequent :: ProofSequentParser Sequent
-- sequent = token $ choice 
--     [ symbol "if" >> do
--         condition <- parens $ many $ noneOf ")"
--         thenSequents <- braces $ many sequent
--         elseSequents <- option [] (symbol "else" >> braces (many sequent))
--         return $ 
--             IfThenElse condition thenSequents elseSequents
--     , symbol "while" >> do
--         condition <- parens $ many $ noneOf ")"
--         whileSequents <- braces $ many sequent
--         return $ While condition whileSequents
--     , assignment
--     ]

-- assignment :: ProofSequentParser Sequent
-- assignment = do
--     p <- use numberType
--     choice 
--         [ do 
--             a <- p
--             var <- token $ some alphaNum <?> "variable-name"
--             createVar var a
--             _ <- symbol ":="
--             expr <- manyTill anyChar semi
--             return $ Assignment var expr
--         , do
--             var <- token $ some alphaNum <?> "variable-name"
--             _ <- symbol ":="
--             expr <- manyTill anyChar semi
--             return $ Assignment var expr
--         ]

-- commentStart :: ProofSequentParser ()
-- commentStart = symbol "//" >> return ()

-- parserNumberType :: ProofSequentParser NumberType
-- parserNumberType = choice
--     [ symbol "Integer" >> return Integer
--     , symbol "Word8" >> return Word8
--     , symbol "Word16" >> return Word16
--     , symbol "Word32" >> return Word32
--     , symbol "Word64" >> return Word64
--     , symbol "Int8" >> return Int8
--     , symbol "Int16" >> return Int16
--     , symbol "Int32" >> return Int32 
--     , symbol "Int64" >> return Int64
--     ]

-- pseudocode :: ProofState
-- pseudocode = ProofState empty commentStart parserNumberType
