{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module FirstOrderLogic.Syntax where

-- When you get the change change TextShow hack to Data.ByteString.Builder
-- import Data.Text (unpack, Text)
-- import Data.HashMap.Strict (lookup, HashMap, insert, insertWith)
-- import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Trans.State (StateT)
-- import Control.Lens (makeLenses, use, uses, (%=), (+=), traversed)
-- import Text.Trifecta (Parser)
-- import Data.SBV (SymWord, SBool, Symbolic, SBV, SInteger)
-- import Data.SBV.Internals (Quantifier(..))
-- import Prelude hiding (lookup)

-- type SBVParser a = StateT (ParserState a) Parser

-- data LanguageDef a = LanguageDef {
--     _mathOperation :: SBVParser (a -> a -> a)
--   , _ordAndEq :: HashMap String (SInteger -> SInteger -> SBool)
--   , _prepositionalLogic :: HashMap String (SBool -> SBool -> SBool)
--   }

-- data ParserState a = ParserState {
--       _languageDef :: LanguageDef a
--     , _varToUniqueVersion :: HashMap String SInteger
--     , _uniqueVarToInfo :: HashMap Text VariableInfo
--     }

-- data VariableInfo = VariableInfo {
--       -- _symbolicType :: NumberType
--     _Name :: Text
--     , _symbolicVersion :: Quantifier
--     }

-- $(makeLenses ''LanguageDef)
-- $(makeLenses ''ParserState)
-- $(makeLenses ''VariableInfo)

--
--
--
--
--
--
-- May remove the above
--
--
--
--
--
--


-- getUniqueVar :: FormulaeParser a UniqueVar
-- getUniqueVar = do 
--   newVar <- use nextUniqueVar
--   nextUniqueVar += 1
--   return newVar

-- createVar :: Text -> Text -> SBVParser a ()
-- createVar numType var = do
--   maybeType <- uses (languageDef.numberType) $ lookup numType
--   case maybeType of
--     Nothing -> lift $ fail $ mconcat ["There is no such type: ", unpack numType]
--     Just a -> do
--       uniqueName <- getUniqueVar
--       variableInfo %= insertWith (++) var [(VariableInfo a uniqueName EX)]

-- lookupOp :: String -> SBVParser a (Maybe (a -> a -> a))
-- lookupOp op = do
--   maybeType <- uses (languageDef.mathOperation) $ lookup op
--   return maybeType

-- lookupVar :: Text -> SBVParser
-- lookupVar = undefined

type Condition = Formulae (Expr Integer)

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
              | Formulae a :~& Formulae a
              | Formulae a :~| Formulae a
              | Formulae a :<+> Formulae a
              | Formulae a :<=> Formulae a
              | Star (Formulae a) (Formulae a) 
              | Formulae a :-* Formulae a 
              | Forall [String] (Formulae a) 
              | Exists [String] (Formulae a) 
    deriving (Show, Functor, Traversable, Foldable)

data Expr a = Var String
          | Num a
          | Expr a :+ Expr a
          | Expr a :- Expr a
          | Expr a :* Expr a
          | Expr a :^ Expr a
          | Quot (Expr a) (Expr a)
          | Rem (Expr a) (Expr a)
          | Div (Expr a) (Expr a)
          | Mod (Expr a) (Expr a)
    deriving (Show, Functor, Traversable, Foldable)
