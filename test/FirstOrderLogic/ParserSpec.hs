module FirstOrderLogic.ParserSpec (spec) where

import Test.Hspec
--import FirstOrderLogic.Parser
--import FirstOrderLogic.Syntax
--import Test.QuickCheck 
--import Text.Trifecta
--import Text.PrettyPrint.ANSI.Leijen


spec :: Spec
spec = do
  describe "test" $ do
    it "should" $ do
        pendingWith "Testing is usefull, but time consuming"
      --   \x -> parseString parseFOL mempty 
      --       (show (displayS (renderCompact (pretty x)))) == x
            
-- instance (Arbitrary a, Arbitrary b) => Arbitrary (Expr a b) where
--         arbitrary
--           = do x <- choose (0 :: Int, 8)
--                case x of
--                    0 -> do x1 <- arbitrary
--                            return (Var x1)
--                    1 -> do x1 <- arbitrary
--                            return (Num x1)
--                    2 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return ((:+) x1 x2)
--                    3 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return ((:-) x1 x2)
--                    4 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return ((:*) x1 x2)
--                    5 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return (Quot x1 x2)
--                    6 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return (Rem x1 x2)
--                    7 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return (Div x1 x2)
--                    8 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return (Mod x1 x2)
--                    _ -> error "FATAL ERROR: Arbitrary instance, logic bug"


-- instance Arbitrary a => Arbitrary (Formulae a) where
--         arbitrary
--           = do x <- choose (0 :: Int, 18)
--                case x of
--                    0 -> do x1 <- arbitrary
--                            return (Lit x1)
--                    1 -> return Emp
--                    2 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return ((:|->) x1 x2)
--                    3 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return ((:==) x1 x2)
--                    4 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return ((:/=) x1 x2)
--                    5 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return ((:>) x1 x2)
--                    6 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return ((:<) x1 x2)
--                    7 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return ((:<=) x1 x2)
--                    8 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return ((:>=) x1 x2)
--                    9 -> do x1 <- arbitrary
--                            return (Not x1)
--                    10 -> do x1 <- arbitrary
--                             x2 <- arbitrary
--                             return ((:&) x1 x2)
--                    11 -> do x1 <- arbitrary
--                             x2 <- arbitrary
--                             return ((:|) x1 x2)
--                    12 -> do x1 <- arbitrary
--                             x2 <- arbitrary
--                             return ((:->) x1 x2)
--                    13 -> do x1 <- arbitrary
--                             x2 <- arbitrary
--                             return ((:~&) x1 x2)
--                    14 -> do x1 <- arbitrary
--                             x2 <- arbitrary
--                             return ((:~|) x1 x2)
--                    15 -> do x1 <- arbitrary
--                             x2 <- arbitrary
--                             return ((:<+>) x1 x2)
--                    16 -> do x1 <- arbitrary
--                             x2 <- arbitrary
--                             return ((:<=>) x1 x2)
--                    17 -> do x1 <- arbitrary
--                             x2 <- arbitrary
--                             return (Star x1 x2)
--                    18 -> do x1 <- arbitrary
--                             x2 <- arbitrary
--                             return ((:-*) x1 x2)
--                    _ -> error "FATAL ERROR: Arbitrary instance, logic bug"

-- instance Arbitrary a => Arbitrary (Quantifier a) where
--         arbitrary
--           = do x <- choose (0 :: Int, 2)
--                case x of
--                    0 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return (Forall x1 x2)
--                    1 -> do x1 <- arbitrary
--                            x2 <- arbitrary
--                            return (Exists x1 x2)
--                    2 -> do x1 <- arbitrary
--                            return (Formulae x1)
--                    _ -> error "FATAL ERROR: Arbitrary instance, logic bug"
