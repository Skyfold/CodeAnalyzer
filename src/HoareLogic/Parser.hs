
module HoareLogic.Parser (readProof) where

import Text.Trifecta
import HoareLogic.Structure
import FirstOrderLogic.Parser (parseVariable, parseExpr, parseFormulae, parseFOL)
import FirstOrderLogic.Syntax (Condition, FOL, Formulae (..), Quantifier (..))
import Control.Monad.IO.Class (MonadIO) 
import qualified Data.Set as Set

-- | Read proof file
readProof :: MonadIO m => String -> m (Maybe ProofSequent)
readProof = parseFromFile proofSequent

proofSequent :: (Monad m, TokenParsing m) => m ProofSequent
proofSequent = do
    whiteSpace
    (preCon, postCon, listOfInvariants) <- annotation
    listOfSequents <- some sequent
    return $ 
        ProofSequent 
            (fst <$> listOfSequents)
            postCon 
            preCon
            listOfInvariants
            ((Set.unions (snd <$> listOfSequents)))


annotation :: (Monad m, TokenParsing m) => m (FOL, FOL, [Condition])
annotation = commentStart >> symbol "{-@" >> do
    preCon <- option (Formulae (Lit True)) (try $ do 
        l <- parseFOL <?> "precondition" 
        _ <- symbol "#"
        return l)
    postCon <- parseFOL <?> "postcondition"
    listOfInvariants <- many $ 
        (symbol "@@" >> parseFormulae <?> "loop invariant")
    _ <- symbol "@-}"
    return $ (preCon, postCon, listOfInvariants)

sequent :: (Monad m, TokenParsing m) => m (Sequent, Set.Set VariableName)
sequent = token $ choice 
    [ symbol "if" >> do
        condition <- parens parseFormulae
        thenSequents <- braces $ many sequent
        elseSequents <- option [] (symbol "else" >> braces (many sequent))
        return 
            (IfThenElse 
                condition 
                (fst <$> thenSequents) 
                (fst <$> elseSequents), 
                (Set.unions (snd <$> thenSequents ++ elseSequents)))
    , symbol "while" >> do
        condition <- parens parseFormulae
        whileSequents <- braces $ many sequent
        return (While 
                  condition 
                  (fst <$> whileSequents),
                  (Set.unions (snd <$> whileSequents)))
    , do
        v <- parseVariable <* symbol ":="
        expr <- parseExpr <* semi
        return $ (Assignment v expr, Set.singleton v)
    ]

commentStart :: (Monad m, TokenParsing m) => m ()
commentStart = symbol "//" >> return ()
