{-# LANGUAGE TemplateHaskell #-}

module HoareLogic.Structure 
    ( ProofSequent (..)
    , Sequent (..)
    , VariableName
    , sequents
    , postCondition
    , loopInvariant 
    , allVariables
    , preCondition
    ) where

import Control.Lens (makeLenses)
import Prelude hiding (lookup)
import FirstOrderLogic.Syntax
import Data.Set (Set)


data ProofSequent = ProofSequent {
      _sequents :: [Sequent]
    , _postCondition :: FOL
    , _preCondition :: FOL
    , _loopInvariant :: [Condition]
    , _AllVariables :: Set VariableName
    }

data Sequent = IfThenElse Condition [Sequent] [Sequent]
             | While Condition [Sequent]
             | Assignment VariableName (SBVExpr) 

$(makeLenses ''ProofSequent)
