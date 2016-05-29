{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module FirstOrderLogic.Syntax where

import Text.PrettyPrint.ANSI.Leijen

type FOL = Quantifier Condition
type Condition = Formulae SBVExpr
type SBVExpr = Expr VariableName Integer

data Error = NoLoopInvariant
           | MultiQuantifiersForVars VariableName
    deriving Show

type VariableName = String

data Quantifier a = Forall [VariableName] (Quantifier a) 
                  | Exists [VariableName] (Quantifier a) 
                  | Formulae a
    deriving (Show, Functor, Traversable, Foldable)

data Formulae a = Lit Bool
              | Emp
              | a :|-> [a]
              | a :== a 
              | a :/= a 
              | a :> a
              | a :< a
              | a :<= a
              | a :>= a
              | Not (Formulae a) 
              | Formulae a :& Formulae a 
              | Formulae a :| Formulae a 
              | Formulae a :-> Formulae a 
              | Formulae a :~& Formulae a
              | Formulae a :~| Formulae a
              | Formulae a :<+> Formulae a
              | Formulae a :<=> Formulae a
              | Star (Formulae a) (Formulae a) 
              | Formulae a :-* Formulae a 
    deriving (Show, Functor, Traversable, Foldable)

data Expr a b = Var a
              | Num b
              | Expr a b :+ Expr a b
              | Expr a b :- Expr a b
              | Expr a b :* Expr a b
              | Expr a b :^ Expr a b
              | Quot (Expr a b) (Expr a b)
              | Rem (Expr a b) (Expr a b)
              | Div (Expr a b) (Expr a b)
              | Mod (Expr a b) (Expr a b)
    deriving (Show, Functor, Traversable, Foldable)

mapVar :: (a -> Expr a b) -> Expr a b -> Expr a b
mapVar f expr = case expr of
    Var a -> f a
    Num b -> Num b
    a :+ b -> mapVar f a :+ mapVar f b
    a :- b -> mapVar f a :- mapVar f b
    a :* b -> mapVar f a :* mapVar f b
    a :^ b -> mapVar f a :^ mapVar f b
    Quot a b -> Quot (mapVar f a) (mapVar f b)
    Rem a b -> Rem (mapVar f a) (mapVar f b)
    Div a b -> Div (mapVar f a) (mapVar f b)
    Mod a b -> Mod (mapVar f a) (mapVar f b)
    

instance (Pretty a) => Pretty (Formulae a) where
    pretty formulae = case formulae of
        Lit a -> pretty a
        Emp -> text "emp"
        a :|-> b -> parens $ pretty a <+> text "|->" <+> pretty b
        a :== b -> parens $ pretty a <+> text "=" <+> pretty b
        a :/= b -> parens $ pretty a <+> text "/=" <+> pretty b
        a :> b -> parens $ pretty a <+> text ">" <+> pretty b
        a :< b -> parens $ pretty a <+> text "<" <+> pretty b
        a :<= b -> parens $ pretty a <+> text "<=" <+> pretty b
        a :>= b -> parens $ pretty a <+> text ">=" <+> pretty b
        Not a -> text "~" <> pretty a
        a :& b -> parens $ pretty a <+> text "&&" <+> pretty b
        a :| b -> parens $ pretty a <+> text "||" <+> pretty b
        a :-> b -> parens $ pretty a <+> text "->" <+> pretty b
        a :~& b -> parens $ pretty a <+> text "~&" <+> pretty b
        a :~| b -> parens $ pretty a <+> text "~|" <+> pretty b
        a :<+> b -> parens $ pretty a <+> text "<+>" <+> pretty b
        a :<=> b -> parens $ pretty a <+> text "<=>" <+> pretty b
        Star a b -> parens $ pretty a <+> text "*" <+> pretty b
        a :-* b -> parens $ pretty a <+> text "-*" <+> pretty b

instance (Pretty a, Pretty b) => Pretty (Expr a b) where
    pretty expr = case expr of
        Var a -> pretty a
        Num b -> pretty b
        (a :+ b) -> parens $ (pretty a) <+> char '+' <+> (pretty b)
        (a :- b) -> parens $ (pretty a) <+> char '-' <+> (pretty b)
        (a :* b) -> parens $ (pretty a) <+> char '*' <+> (pretty b)
        (a :^ b) -> parens $ (pretty a) <+> char '^' <+> (pretty b)
        (Quot a b) -> parens $ (pretty a) <+> text "`quot`" <+> (pretty b)
        (Rem a b) -> parens $ (pretty a) <+> text "`rem`" <+> (pretty b)
        (Div a b) -> parens $ (pretty a) <+> char '/' <+> (pretty b)
        (Mod a b) -> parens $ (pretty a) <+> char '%' <+> (pretty b)

instance Pretty a => Pretty (Quantifier a) where
    pretty a = case a of
        Forall vars inner -> 
                text "forall" 
            <+> encloseSep empty empty comma (fmap pretty vars)
            <+> parens (pretty inner)
        Exists vars inner ->
                text "exists" 
            <+> encloseSep empty empty comma (fmap pretty vars)
            <+> parens (pretty inner)
        Formulae inner -> pretty inner
