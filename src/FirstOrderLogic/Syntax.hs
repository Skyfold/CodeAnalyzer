{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module FirstOrderLogic.Syntax where

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

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

formulaeT :: Applicative f => (a -> f c) -> Formulae a -> f (Formulae c)
formulaeT f expr = case expr of
    Emp -> pure Emp
    Lit a -> pure $ Lit a
    a :|-> b -> (:|->) <$> f a <*> traverse f b
    a :== b  -> (:==) <$> f a <*> f b
    a :/= b -> (:/= ) <$> f a <*> f b
    a :> b -> (:>) <$> f a <*> f b
    a :< b -> (:<) <$> f a <*> f b
    a :<= b -> (:<=) <$> f a <*> f b
    a :>= b -> (:>=) <$> f a <*> f b
    Not a -> Not <$> formulaeT f a
    a :& b -> (:& ) <$> formulaeT f a <*> formulaeT f b
    a :| b -> (:| ) <$> formulaeT f a <*> formulaeT f b
    a :-> b -> (:-> ) <$> formulaeT f a <*> formulaeT f b
    a :~& b -> (:~& ) <$> formulaeT f a <*> formulaeT f b
    a :~| b -> (:~| ) <$> formulaeT f a <*> formulaeT f b
    a :<+> b -> (:<+> ) <$> formulaeT f a <*> formulaeT f b
    a :<=> b -> (:<=> ) <$> formulaeT f a <*> formulaeT f b
    Star a b -> Star <$> formulaeT f a <*> formulaeT f b
    a :-* b -> (:-* ) <$> formulaeT f a <*> formulaeT f b

-- data BinaryFormulae = (:|-> 
--                     | 

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
    deriving (Show, Functor, Traversable, Foldable )

vars :: Applicative f => (a -> f c) -> Expr a b -> f (Expr c b)
vars f expr = case expr of
    Var a -> Var <$> f a
    Num b -> Num <$> pure b
    l :+ r -> (:+) <$> vars f l <*> vars f r
    l :- r -> (:-) <$> vars f l <*> vars f r
    l :* r -> (:*) <$> vars f l <*> vars f r
    Quot l r -> Quot <$> vars f l <*> vars f r 
    Rem l r -> Rem <$> vars f l <*> vars f r   
    Div l r -> Div <$> vars f l <*> vars f r 
    Mod l r -> Mod <$> vars f l <*> vars f r 

data Expr a b = Var a
              | Num b
              | Expr a b :+ Expr a b
              | Expr a b :- Expr a b
              | Expr a b :* Expr a b
              -- | Expr a b :^ Expr a b
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
   -- a :^ b -> mapVar f a :^ mapVar f b
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
       -- (a :^ b) -> parens $ (pretty a) <+> char '^' <+> (pretty b)
        (Quot a b) -> parens $ (pretty a) <+> text "`quot`" <+> (pretty b)
        (Rem a b) -> parens $ (pretty a) <+> text "`rem`" <+> (pretty b)
        (Div a b) -> parens $ (pretty a) <+> char '/' <+> (pretty b)
        (Mod a b) -> parens $ (pretty a) <+> char '%' <+> (pretty b)

instance Pretty a => Pretty (Quantifier a) where
    pretty a = case a of
        Forall variables inner -> 
                text "forall" 
            <+> encloseSep empty empty comma (fmap pretty variables)
            <+> parens (pretty inner)
        Exists variables inner ->
                text "exists" 
            <+> encloseSep empty empty comma (fmap pretty variables)
            <+> parens (pretty inner)
        Formulae inner -> pretty inner
