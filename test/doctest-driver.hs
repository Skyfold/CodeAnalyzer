import Test.DocTest

main :: IO ()
main = do
    firstOrderLogic_Parser
    fOASL_Converter
    hoareLogic_Parser
    weakestPrecondition_Syntax

firstOrderLogic_Parser :: IO ()
firstOrderLogic_Parser = doctest ["-isrc", "src/FirstOrderLogic/Parser.hs"]

fOASL_Converter :: IO ()
fOASL_Converter = doctest ["-isrc", "src/FOASL/Converter.hs"]

hoareLogic_Parser :: IO ()
hoareLogic_Parser = doctest ["-isrc", "src/HoareLogic/Parser.hs"]

weakestPrecondition_Syntax :: IO ()
weakestPrecondition_Syntax = doctest ["-isrc", "src/WeakestPrecondition/Syntax.hs"]
