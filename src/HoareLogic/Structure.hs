{-# LANGUAGE TemplateHaskell #-}

module HoareLogic.Structure where

-- import Data.HashMap.Strict (lookup, HashMap, insert)
import Control.Lens (makeLenses, use, uses, (%=), (+=), traversed)
-- import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Trans.State (StateT)
-- import Text.Trifecta (Parser, unexpected)
import Prelude hiding (lookup)
import FirstOrderLogic.Syntax


data ProofSequent = ProofSequent {
      _sequents :: [Sequent]
    , _postCondition :: Condition
    , _loopInvariant :: Condition
    }
  deriving Show

data Sequent = IfThenElse Condition [Sequent] [Sequent]
             | While Condition [Sequent]
             | Assignment String (Expr Integer) 
    deriving Show

$(makeLenses ''ProofSequent)
