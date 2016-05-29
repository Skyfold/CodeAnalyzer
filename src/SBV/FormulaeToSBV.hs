{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module SBV.FormulaeToSBV (checkCondition) where

import FirstOrderLogic.Syntax
import Data.SBV
import qualified Data.SBV.Internals as SBVi
import Data.HashMap.Strict 
import Control.Monad.State.Strict
import Control.Monad.Except
import Prelude hiding (lookup, map)

type MapSInteger = HashMap String SInteger
type MapQuan = HashMap String (Maybe SBVi.Quantifier)

-- for testing only
instance Show SBVi.Quantifier where
    show SBVi.ALL = "ALL"
    show SBVi.EX = "EX"

checkCondition :: FOL -> IO (ThmResult)
checkCondition fol = do 
    let (f, map) = folToSBV fol empty
    prove (test empty f (toList map))

test :: MapSInteger -> (MapSInteger -> SBool) -> 
    [(String, Maybe SBVi.Quantifier)] -> Symbolic SBool
test map f list = case list of
    [] -> return $ f map
    (k,v):xs -> do
        sInt <- mkSymWord v (Just k)
        test (union (singleton k sInt) map) f xs

folToSBV :: FOL -> MapQuan -> (MapSInteger -> SBool, MapQuan)
folToSBV fol map = case fol of
    Forall vars inner -> 
        folToSBV 
            inner
            (unionWith 
                combineQ
                (fromList ((,Just SBVi.ALL) <$> vars))
                map
            )
    Exists vars inner ->
        folToSBV 
            inner
            (unionWith 
                combineQ
                (fromList ((,Just SBVi.EX) <$> vars))
                map
            )
    Formulae inner -> runState (formulaeToSBV inner) map

combineQ :: Maybe SBVi.Quantifier -> Maybe SBVi.Quantifier -> Maybe SBVi.Quantifier
combineQ _ (Just SBVi.ALL) = Just SBVi.ALL
combineQ (Just SBVi.ALL) _ = Just SBVi.ALL
combineQ Nothing a = a
combineQ a Nothing = a
combineQ _ _ = Nothing

formulaeToSBV :: Condition -> State MapQuan (MapSInteger -> SBool)
formulaeToSBV formulae = case formulae of
    (Lit a) -> return (\_ -> fromBool a)
    (l :== r) -> do 
        a <- exprToSBV l
        b <- exprToSBV r
        return (\z -> (.==) (a z) (b z))
    (l :/= r) -> do
        a <- exprToSBV l
        b <- exprToSBV r
        return (\z -> (./=) (a z) (b z))
    (l :> r) -> do
        a <- exprToSBV l
        b <- exprToSBV r
        return (\z -> (.>) (a z) (b z))
    (l :< r) -> do
        a <- exprToSBV l
        b <- exprToSBV r
        return (\z -> (.<) (a z) (b z))
    (l :<= r) -> do
        a <- exprToSBV l
        b <- exprToSBV r
        return (\z -> (.<=) (a z) (b z))
    (l :>= r) -> do
        a <- exprToSBV l
        b <- exprToSBV r
        return (\z -> (.>=) (a z) (b z))
    (Not c) -> do
        a <- formulaeToSBV c
        return (\z -> bnot (a z))
    (l :& r) -> do
        a <- formulaeToSBV l
        b <- formulaeToSBV r
        return (\z -> (&&&) (a z) (b z))
    (l :| r) -> do
        a <- formulaeToSBV l
        b <- formulaeToSBV r
        return (\z -> (|||) (a z) (b z))
    (l :-> r) -> do
        a <- formulaeToSBV l
        b <- formulaeToSBV r
        return (\z -> (==>) (a z) (b z))
    (l :~& r) -> do
        a <- formulaeToSBV l
        b <- formulaeToSBV r
        return (\z -> (~&) (a z) (b z))
    (l :~| r) -> do
        a <- formulaeToSBV l
        b <- formulaeToSBV r
        return (\z -> (~|) (a z) (b z))
    (l :<+> r) -> do
        a <- formulaeToSBV l
        b <- formulaeToSBV r
        return (\z -> (<+>) (a z) (b z))
    (l :<=> r) -> do
        a <- formulaeToSBV l
        b <- formulaeToSBV r
        return (\z -> (<=>) (a z) (b z))
    _ -> error "Not implemented in formulaeToSBV yet"

exprToSBV :: SBVExpr -> State MapQuan (MapSInteger -> SInteger)
exprToSBV expr = case expr of
  (Var a) -> do 
    modify (union (singleton a Nothing))
    return (\h -> maybe (error "will never happen") id (lookup a h))
  (Num b) -> return (\_ -> fromInteger b)
  (l :+ r) -> do 
    a <- exprToSBV l
    b <- exprToSBV r
    return (\z -> (+) (a z) (b z))
  (l :- r) -> do 
    a <- exprToSBV l
    b <- exprToSBV r
    return (\z -> (-) (a z) (b z))
  (l :* r) -> do 
    a <- exprToSBV l
    b <- exprToSBV r
    return (\z -> (*) (a z) (b z))
  (Quot l r) -> do
    a <- exprToSBV l
    b <- exprToSBV r
    return (\z -> sQuot (a z) (b z))
  (Rem l r) -> do
    a <- exprToSBV l
    b <- exprToSBV r
    return (\z -> sRem (a z) (b z))
  (Div l r) -> do
    a <- exprToSBV l
    b <- exprToSBV r
    return (\z -> sDiv (a z) (b z))
  (Mod l r) -> do
    a <- exprToSBV l
    b <- exprToSBV r
    return (\z -> sMod (a z) (b z))
  (l :^ r) -> do
    a <- exprToSBV l
    b <- exprToSBV r
    return (\z -> (.^) (a z) (b z))

-- getVarFormulae :: Condition -> mapQuanQuan -> mapQuanQuan
-- getVarFormulae con mapQuan = case con of
--     (l :== r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--     (l :/= r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--     (l :> r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--     (l :< r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--     (l :<= r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--     (l :>= r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--     (Not c) -> getVarFormulae c mapQuan
--     (l :& r) -> union (getVarFormulae l mapQuan) (getVarFormulae r mapQuan)
--     (l :| r) -> union (getVarFormulae l mapQuan) (getVarFormulae r mapQuan)
--     (l :-> r) -> union (getVarFormulae l mapQuan) (getVarFormulae r mapQuan)
--     (l :~& r) -> union (getVarFormulae l mapQuan) (getVarFormulae r mapQuan)
--     (l :~| r) -> union (getVarFormulae l mapQuan) (getVarFormulae r mapQuan)
--     (l :<+> r) -> union (getVarFormulae l mapQuan) (getVarFormulae r mapQuan)
--     (l :<=> r) -> union (getVarFormulae l mapQuan) (getVarFormulae r mapQuan)
--     _ -> mapQuan

-- getVarExpr :: SBVExpr -> mapQuanQuan -> mapQuanQuan
-- getVarExpr expr mapQuan = case expr of
--   (Var a) -> singleton a Nothing
--   (Num _) -> empty
--   (l :+ r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--   (l :- r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--   (l :* r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--   (Quot l r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--   (Rem l r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--   (Div l r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--   (Mod l r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
--   (l :^ r) -> union (getVarExpr l mapQuan) (getVarExpr r mapQuan)
        
-- formulaeToSBV :: MapSymNum -> Condition -> Symbolic SBool
-- formulaeToSBV mapSym con = do 
--     let mapSInt = map () mapSym
--     case con of
--         Lit a -> return (fromBool a)
--         l :== r -> do 
--             a <- exprToSBV mapSInt l
--             b <- exprToSBV mapSInt r
--             return $ (.==) a b
--         l :/= r -> do
--             a <- exprToSBV mapSInt l
--             b <- exprToSBV mapSInt r
--             return $ (./=) a b
--         l :> r -> do
--             a <- exprToSBV mapSInt l
--             b <- exprToSBV mapSInt r
--             return $ (.>) a b
--         l :< r -> do
--             a <- exprToSBV mapSInt l
--             b <- exprToSBV mapSInt r
--             return $ (.<) a b
--         l :<= r -> do
--             a <- exprToSBV mapSInt l
--             b <- exprToSBV mapSInt r
--             return $ (.<=) a b
--         l :>= r -> do
--             a <- exprToSBV mapSInt l
--             b <- exprToSBV mapSInt r
--             return $ (.>=) a b
--         (Not c) -> do
--             a <- formulaeToSBV mapSInt c
--             return $ bnot a
--         l :& r -> do
--             a <- formulaeToSBV mapSInt l
--             b <- formulaeToSBV mapSInt r
--             return $ (&&&) a b
--         l :| r -> do
--             a <- formulaeToSBV mapSInt l
--             b <- formulaeToSBV mapSInt r
--             return $ (|||) a b
--         l :-> r -> do
--             a <- formulaeToSBV mapSInt l
--             b <- formulaeToSBV mapSInt r
--             return $ (==>) a b
--         l :~& r -> do
--             a <- formulaeToSBV mapSInt l
--             b <- formulaeToSBV mapSInt r
--             return $ (~&) a b
--         l :~| r -> do
--             a <- formulaeToSBV mapSInt l
--             b <- formulaeToSBV mapSInt r
--             return $ (~|) a b
--         l :<+> r -> do
--             a <- formulaeToSBV mapSInt l
--             b <- formulaeToSBV mapSInt r
--             return $ (<+>) a b
--         l :<=> r -> do
--             a <- formulaeToSBV mapSInt l
--             b <- formulaeToSBV mapSInt r
--             return $ (<=>) a b
--         _ -> error "not implemented in formulaeToSBV"

-- exprToSBV :: mapSIntSInteger -> SBVExpr -> SInteger
-- exprToSBV mapSInt expr = case expr of
--   (Var a) -> 
--     maybe 
--         (error "will never happen") 
--         id
--         (lookup a mapSInt)
--   (Num b) -> return (fromInteger b)
--   l :+ r -> do 
--     a <- exprToSBV mapSInt l
--     b <- exprToSBV mapSInt r
--     return $ (+) a b
--   l :- r -> do 
--     a <- exprToSBV mapSInt l
--     b <- exprToSBV mapSInt r
--     return $ (-) a b
--   l :* r -> do 
--     a <- exprToSBV mapSInt l
--     b <- exprToSBV mapSInt r
--     return $ (*) a b
--   Quot l r -> do
--     a <- exprToSBV mapSInt l
--     b <- exprToSBV mapSInt r
--     return $ sQuot a b
--   Rem l r -> do
--     a <- exprToSBV mapSInt l
--     b <- exprToSBV mapSInt r
--     return $ sRem a b
--   Div l r -> do
--     a <- exprToSBV mapSInt l
--     b <- exprToSBV mapSInt r
--     return $ sDiv a b
--   Mod l r -> do
--     a <- exprToSBV mapSInt l
--     b <- exprToSBV mapSInt r
--     return $ sMod a b
--   l :^ r -> do
--     a <- exprToSBV mapSInt l
--     b <- exprToSBV mapSInt r
--     return $ (.^) a b

-- exprToSBV :: Expr Integer -> MapSymNum -> Symbolic SIntegemap r
-- exprToSBV expr map = case expr omap f
--     Var a -> maybe 
--         (error "Put a quantifier for: " ++ a)  
--         ()
--         (lookup a map)

    -- (Forall vars c) -> do
    --     modify (union (fromList ((,Just ALL) <$> vars)))
    --     h <- get
    --     let (f, newH) = runState (formulaeToSBV c) h
    --     modify (unionWith (\new old -> if new /= old then Just ALL else new) newH)
    --     return f
    -- (Exists vars c) -> do
    --     modify (union (fromList ((,Just EX) <$> vars)))
    --     h <- get
    --     let (f, newH) = runState (formulaeToSBV c) h
    --     modify (unionWith (\new old -> if new /= old then Just ALL else new) newH)
    --     return f
