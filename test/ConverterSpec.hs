{-# LANGUAGE OverloadedStrings #-}

module ConverterSpec where

import           Test.Hspec
import           Data.Text (Text)
import           Converter
import           Turtle (view, sh, fold, Text)
import           Control.Foldl (list)

spec :: Spec
spec = do
  describe "test" $ do
    it "should" $ do
        let test = genText $ Not ((Star (Not Emp) (Not Emp)) :-* (Lit False))
        (fold (solveWithFOASL test) list) `shouldReturn` [True]
