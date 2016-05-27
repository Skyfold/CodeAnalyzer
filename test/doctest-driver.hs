import Test.DocTest

main :: IO ()
main = do
    firstOrderLogic_Parser
    fOASL_Converter

firstOrderLogic_Parser :: IO ()
firstOrderLogic_Parser = doctest ["-isrc", "src/FirstOrderLogic/Parser.hs"]

fOASL_Converter :: IO ()
fOASL_Converter = doctest ["-isrc", "src/FOASL/Converter.hs"]
