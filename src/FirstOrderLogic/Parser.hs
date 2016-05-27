{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs #-}

module FirstOrderLogic.Parser (parseExpr, parseFormulae)  where

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
-- >>> parseString withText mempty "y1"
-- Success "y1"
--
-- >>> parseString withText mempty "y1\n \n  "
-- Success "y1"
--
-- >>> parseString withText mempty "bla9000\n\n "
-- Success "bla9000"
withText :: (Monad m, TokenParsing m) => m String
withText = token $ do
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
-- >>> parseString parseExpr mempty "(5 div 6) - a"
-- Success (Div (Num 5) (Num 6) :- Var "a")
--
parseExpr :: (Monad m, TokenParsing m) => m (Expr Integer)
parseExpr = buildExpressionParser exprTable term
    <?> "expression"

term :: (Monad m, TokenParsing m) => m (Expr Integer)
term = parens parseExpr
    <|> (integer >>= (\a -> return (Num a)))
    <|> (withText >>= (\a -> return (Var a)))
    <?> "simple expression"

exprTable :: (Monad m, TokenParsing m) => [[Operator m (Expr Integer)]]
exprTable = [ [prefix "-"   (fmap negate), prefix "+" id ]
            , [binary "^"   (:^) AssocLeft]
            , [binary "*"   (:*) AssocLeft, binary "quot" Quot AssocLeft
            ,  binary "rem" Rem  AssocLeft, binary "div"  Div  AssocLeft
            ,  binary "mod" Mod  AssocLeft]
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
-- >>> parseString parseFormulae mempty "y1 > y2"
-- Success (Var "y1" :> Var "y2")
-- 
-- >>> parseString parseFormulae mempty "(y1 = 6) && (y1 < y2)"
-- Success ((Var "y1" :== Num 6) :& (Var "y1" :< Var "y2"))
--
-- >>> parseString parseFormulae mempty "~((~emp * ~emp) -* False)"
-- Success (Not (Star (Not Emp) (Not Emp) :-* Lit False))
--
-- >>> parseString parseFormulae mempty "((fun <= boo) || (boo >= fun)) -> ~(fun = boo)"
-- Success (((Var "fun" :<= Var "boo") :| (Var "boo" :>= Var "fun")) :-> Not (Var "fun" :== Var "boo"))
--
-- >>> parseString parseFormulae mempty "(y1 + 6) > (y1 + 5)"
-- Success ((Var "y1" :+ Num 6) :> (Var "y1" :+ Num 5))
--
parseFormulae :: (Monad m, TokenParsing m) => m Condition
parseFormulae = buildExpressionParser formulaeTable formulae
    <?> "Formulae in FOL"

formulae :: (Monad m, TokenParsing m) => m Condition
formulae = choice [ try $ parens parseFormulae
                  , literal
                  , parseExprConnective parseExpr
                  ]

formulaeTable :: (Monad m, TokenParsing m) => [[Operator m Condition]]
formulaeTable = 
    [ [forall, exists ]  
    , [ prefix "~" Not]
    , [ binary "&&" (:&) AssocRight]
    , [ binary "||" (:|) AssocRight]
    , [ binary "->" (:->) AssocRight]
    , [ binary "<->" (:<=>) AssocRight]
    , [ binary "nand" (:~&) AssocRight, binary "nor" (:~|) AssocRight
    ,   binary "xor" (:<+>) AssocRight ]
    , [ binary "-*" (:-*) AssocRight, binary "*" Star AssocRight]
    ]

forall :: (Monad m, TokenParsing m) => Operator m Condition
forall = Prefix (Forall <$> (symbol "forall" >> withText `sepBy1` symbolic ','))

exists :: (Monad m, TokenParsing m) => Operator m Condition
exists = Prefix (Exists <$> (symbol "exists" >> withText `sepBy1` symbolic ','))

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
-- >>> parseString (parseExprConnective withText) mempty "y1 > y2" :: Result (Formulae String)
-- Success ("y1" :> "y2")
--
-- >>> parseString (parseExprConnective withText) mempty "y1 \n \n=\n  y2  \n" :: Result (Formulae String)
-- Success ("y1" :== "y2")
--
-- >>> parseString (parseExprConnective withText) mempty "y1 \n \n|->\n  y2  \n" :: Result (Formulae String)
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
        , textSymbol "<=" >> exprParser >>= (\b -> return $ l :<= b)
        , textSymbol ">=" >> exprParser >>= (\b -> return $ l :>= b)
        , textSymbol ">" >> exprParser  >>= (\b -> return $ l :> b)
        , textSymbol "<" >> exprParser  >>= (\b -> return $ l :< b)
        ] 
