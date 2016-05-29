{-# LANGUAGE TemplateHaskell #-}

module HoareLogic.Structure 
    ( ProofSequent (..)
    , Sequent (..)
    , VariableName
    , sequents
    , postCondition
    , loopInvariant 
    ) where

import Control.Lens (makeLenses)
import Prelude hiding (lookup)
import FirstOrderLogic.Syntax
import Data.SBV (Symbolic, SInteger)
import Data.Set (Set)


data ProofSequent = ProofSequent {
      _sequents :: [Sequent]
    , _postCondition :: FOL
    , _loopInvariant :: [Condition]
    , _AllVariables :: Set VariableName
    }

data Sequent = IfThenElse Condition [Sequent] [Sequent]
             | While Condition [Sequent]
             | Assignment VariableName (SBVExpr) 

$(makeLenses ''ProofSequent)
