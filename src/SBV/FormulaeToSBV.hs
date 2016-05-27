{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module SBV.FormulaeToSBV (formulaeToSBV) where

import FirstOrderLogic.Syntax
import Data.SBV
import Data.HashMap.Strict (lookup, HashMap, union, fromList)
import Prelude hiding (lookup)

type SBVstate = HashMap String (Symbolic SInteger)

formulaeToSBV :: Formulae (Expr SInteger) -> SBVstate -> Symbolic SBool
formulaeToSBV formulae h = case formulae of
    (Lit a) -> return $ fromBool a
    (l :== r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ a .== b}
    (l :> r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ a .> b}
    (l :< r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ a .< b}
    (l :<= r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ a .<= b}
    (l :>= r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ a .>= b}
    (Not c) -> do {a <- formulaeToSBV c h; return $ bnot a}
    (l :& r) -> 
        do {a <- formulaeToSBV l h; b <- formulaeToSBV r h; return $ a &&& b} 
    (l :| r) -> 
        do {a <- formulaeToSBV l h; b <- formulaeToSBV r h; return $ a ||| b}
    (l :-> r) -> 
        do {a <- formulaeToSBV l h; b <- formulaeToSBV r h; return $ a ==> b}
    (l :~& r) ->
        do {a <- formulaeToSBV l h; b <- formulaeToSBV r h; return $ a ~& b}
    (l :~| r) ->
        do {a <- formulaeToSBV l h; b <- formulaeToSBV r h; return $ a ~| b}
    (l :<+> r) -> 
        do {a <- formulaeToSBV l h; b <- formulaeToSBV r h; return $ a <+> b}
    (l :<=> r) ->
        do {a <- formulaeToSBV l h; b <- formulaeToSBV r h; return $ a <=> b}
    (Forall var c) -> formulaeToSBV c $ 
        union (fromList (zip var (forall <$> var))) h
    _ -> error "Not implemented in formulaeToSBV yet"

exprToSBV :: Expr SInteger -> SBVstate -> Symbolic SInteger
exprToSBV expr h = case expr of
    (Var a) -> maybe (exists a) id (lookup a h)
    (Num b) -> return b
    (l :+ r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ a + b}
    (l :- r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ a - b}
    (l :* r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ a * b}
    (Quot l r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ sQuot a b}
    (Rem l r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ sRem a b}
    (Div l r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ sDiv a b}
    (Mod l r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ sMod a b}
    (l :^ r) -> do {a <- exprToSBV l h; b <- exprToSBV r h; return $ a .^ b}
