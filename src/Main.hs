{-# LANGUAGE OverloadedStrings #-}

import WeakestPrecondition.Syntax (wkPrecondition)
import SBV.FormulaeToSBV (checkCondition)
import HoareLogic.Parser (readProof)
import System.IO (hPutStrLn, stderr)
import System.Environment (getProgName, getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filepath] -> do
            _ <- readFile filepath
            maybeProofSequent <- readProof filepath
            case maybeProofSequent of
                Nothing -> do 
                    exitFailure
                Just proofSequent -> 
                    case (wkPrecondition proofSequent) of
                        Left a -> do
                            hPutStrLn stderr $ "error: " ++ show a
                            exitFailure
                        Right a -> do 
                            result <- checkCondition a
                            print result
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " <FilePath>"
            exitFailure
