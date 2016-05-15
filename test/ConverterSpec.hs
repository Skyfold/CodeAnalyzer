{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConverterSpec where

import           Test.Hspec
import           Converter
import           Turtle (fold, Text)
import           TextShow (toString, showb)
import           Control.Foldl (list)
import           Data.Monoid ((<>))

spec :: Spec
spec = do
  let test = (Not ((Star (Not Emp) (Not Emp)) 
                    :-* (Lit False))) :: Formulae (Expr Text)
  let test2 = (
              (((Var "y") :== (Var "y1")) :& 
                  ((Var "y1") :|-> [(Var "1")])) 
                  :-> 
              (((Var "y") :== (Var "y1")) :&
                  ((Var "y1") :|-> [(Var "1")]) :&
                  ((Var "y") :|-> [(Var "1")]))
              ) :: Formulae (Expr Text)
  describe ("queries FOASL with \n" <> (toString (showb test))) $ do
    it "should return True" $ do
        (fold (solveWithFOASL test) list) `shouldReturn` [True]
  describe ("queries FOASL with \n" <> (toString (showb test2))) $ do
    it "should return True" $ do
        (fold (solveWithFOASL test2) list) `shouldReturn` [True]
