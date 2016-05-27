{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

module WeakestPrecondition.Syntax where

import Data.SBV (SymWord, SBool, Symbolic, SBV, SInteger)
import Data.SBV.Internals (Quantifier(..))
