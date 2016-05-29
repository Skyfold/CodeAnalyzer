{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs #-}

module FirstOrderLogic.Parser 
    ( parseVariable
    , parseExpr
    , parseFormulae
    , parseFOL
    ) where

import Text.Trifecta hiding ((:^))
import Control.Applicative ((<|>))
import Text.Parser.Token.Style (emptyOps)
import Text.Parser.Expression
import FirstOrderLogic.Syntax
import Prelude hiding (lookup)

-- docktest setup
-- $setup
-- >>> :set -XFlexibleContexts

---------------------------------- Parser a -----------------------------------

-- | @parseVar@ reads alpha-numeric to 'Text'.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parseString parseVariable mempty "y1"
-- Success "y1"
--
-- >>> parseString parseVariable mempty "y1\n \n  "
-- Success "y1"
--
-- >>> parseString parseVariable mempty "bla9000\n\n "
-- Success "bla9000"
parseVariable :: (Monad m, TokenParsing m) => m String
parseVariable = token $ do
    val <- (:) <$> (letter <?> "variables start with letters")
               <*> (many alphaNum)
    return val

------------------------------- Parser (Expr a) -------------------------------
--
-- | @parseExpr@ 
--
-- >>> parseString parseExpr mempty "5 + a"
-- Success (Num 5 :+ Var "a")
--
-- >>> parseString parseExpr mempty "(5 * a)"
-- Success (Num 5 :* Var "a")
--
-- >>> parseString parseExpr mempty "(5 / 6) - a"
-- Success (Div (Num 5) (Num 6) :- Var "a")
--
parseExpr :: (Monad m, TokenParsing m) => m (SBVExpr)
parseExpr = buildExpressionParser exprTable term
    <?> "expression"

term :: (Monad m, TokenParsing m) => m (SBVExpr)
term = parens parseExpr
    <|> (integer >>= (\a -> return (Num a)))
    <|> (parseVariable >>= (\a -> return (Var a)))
    <?> "simple expression"

exprTable :: (Monad m, TokenParsing m) => [[Operator m (SBVExpr)]]
exprTable = [ [prefix "-"   (fmap negate)]
            , [binary "*"   (:*) AssocLeft, binary "`quot`" Quot AssocLeft
            ,  binary "`rem`" Rem  AssocLeft, binary "/"    Div AssocLeft
            ,  binary "%"   Mod  AssocLeft]
            , [binary "+"   (:+) AssocLeft, binary "-"    (:-) AssocLeft ]
            ]

binary :: (Monad m, TokenParsing m) => 
    String -> (a -> a -> a) -> Assoc -> (Operator m a)
binary  name fun assoc = Infix (fun <$ reservedOp name) assoc

prefix :: (Monad m, TokenParsing m) => String -> (a -> a) -> Operator m a
prefix name fun = Prefix (fun <$ reservedOp name)

reservedOp :: (Monad m, TokenParsing m) => String -> m ()
reservedOp name = reserve emptyOps name 

----------------------------- Parser (Formulae a) ----------------------------- 

-- | @parseFormulae@ given something to parse expressinos it will parses 
-- a 'Formulae'
--
-- ==== __Examples__
--
-- >>> parseString parseFOL mempty "(y1 + 6) > (y1 + 5)"
-- Success (Formulae ((Var "y1" :+ Num 6) :> (Var "y1" :+ Num 5)))
--
parseFOL :: (Monad m, TokenParsing m) => m FOL
parseFOL = 
    choice [     Forall 
            <$> (symbol "forall" >> parseVariable `sepBy1` symbolic ',')
            <*> (Formulae <$> parseFormulae)
           ,     Exists 
            <$> (symbol "exists" >> parseVariable `sepBy1` symbolic ',')
            <*> (Formulae <$> parseFormulae)
           , Formulae <$> parseFormulae
           ]

parseFormulae :: (Monad m, TokenParsing m) => m Condition
parseFormulae = buildExpressionParser formulaeTable formulae
    <?> "Formulae"

formulae :: (Monad m, TokenParsing m) => m Condition
formulae = choice [ try $ parens parseFormulae
                  , literal
                  , parseExprConnective parseExpr
                  ]

formulaeTable :: (Monad m, TokenParsing m) => [[Operator m Condition]]
formulaeTable = 
    [ [ prefix "~" Not]
    , [ binary "&&" (:&) AssocRight]
    , [ binary "||" (:|) AssocRight]
    , [ binary "->" (:->) AssocRight]
    , [ binary "<->" (:<=>) AssocRight]
    , [ binary "nand" (:~&) AssocRight, binary "nor" (:~|) AssocRight
    ,   binary "xor" (:<+>) AssocRight ]
    , [ binary "-*" (:-*) AssocRight, binary "*" Star AssocRight]
    ]

literal :: (Monad m, TokenParsing m) => m Condition
literal = (symbol "emp" >> return Emp)
       <|> (symbol "True" >> return (Lit True))
       <|> (symbol "False" >> return (Lit False))

-- | @parseExprConnective@
-- parsers the connectives given some expression parser 
--
-- ==== __Examples__
--
-- Basic usage:
--
-- >>> parseString (parseExprConnective parseVariable) mempty "y1 > y2" :: Result (Formulae String)
-- Success ("y1" :> "y2")
--
-- >>> parseString (parseExprConnective parseVariable) mempty "y1 \n \n=\n  y2  \n" :: Result (Formulae String)
-- Success ("y1" :== "y2")
--
-- >>> parseString (parseExprConnective parseVariable) mempty "y1 \n \n|->\n  y2  \n" :: Result (Formulae String)
-- Success ("y1" :|-> ["y2"])
--
-- Will parse any of the following:
--
-- 'a :|-> [a]'
-- 'a :== a' 
-- 'a :> a'
-- 'a :< a'
-- 'a :<= a'
-- 'a :>= a'
--
parseExprConnective :: (Monad m, TokenParsing m) => m a -> m (Formulae a)
parseExprConnective exprParser = do
    l <- exprParser
    choice 
        [ textSymbol "|->" >> do 
            r <- exprParser `sepBy1` symbolic ','
            return $ l :|-> r
        , textSymbol "=" >> exprParser  >>= (\b -> return $ l :== b)
        , textSymbol "/=" >> exprParser  >>= (\b -> return $ l :/= b)
        , textSymbol "<=" >> exprParser >>= (\b -> return $ l :<= b)
        , textSymbol ">=" >> exprParser >>= (\b -> return $ l :>= b)
        , textSymbol ">" >> exprParser  >>= (\b -> return $ l :> b)
        , textSymbol "<" >> exprParser  >>= (\b -> return $ l :< b)
        ] 
