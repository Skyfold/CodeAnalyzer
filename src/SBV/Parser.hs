{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module SBV.Parser (
    parseSInt64, parseSInt32, parseSInt16, parseSInt8, parseSWord64, parseSWord32, parseSWord16, parseSWord8, parseSInteger
    ) where

import Text.Trifecta as T
import Data.SBV

equality :: EqSymbolic a => Parser a -> Parser SBool
equality parser = do
  l <- parser
  choice [ T.symbolic '=' >> do
            r <- parser
            return $ l .== r
         , symbol "/=" >> do
            r <- parser
            return $ l ./= r
        ]

order :: OrdSymbolic a => Parser a -> Parser SBool
order parser = do
  l <- parser
  choice [ T.symbolic '<' >> do
            r <- parser
            return $ l .< r
         , T.symbolic '>' >> do
            r <- parser
            return $ l .> r
         , symbol "<=" >> do
            r <- parser
            return $ l .<= r
         , symbol ">=" >> do
            r <- parser
            return $ l .>= r
        ]

parseSInteger :: Parser SInteger
parseSInteger = do { x <- integer; return $ fromInteger x }

parseSWord8 :: Parser SWord8
parseSWord8 = do { x <- integer; return $ fromInteger x }

parseSWord16 :: Parser SWord16
parseSWord16 = do { x <- integer; return $ fromInteger x }

parseSWord32 :: Parser SWord32
parseSWord32 = do { x <- integer; return $ fromInteger x }

parseSWord64 :: Parser SWord64
parseSWord64 = do { x <- integer; return $ fromInteger x }

parseSInt8 :: Parser SInt8
parseSInt8 = do { x <- integer; return $ fromInteger x }

parseSInt16 :: Parser SInt16
parseSInt16 = do { x <- integer; return $ fromInteger x }

parseSInt32 :: Parser SInt32
parseSInt32 = do { x <- integer; return $ fromInteger x }

parseSInt64 :: Parser SInt64
parseSInt64 = do { x <- integer; return $ fromInteger x }

