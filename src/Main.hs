-- |
-- Module      : Main
-- Description : Main module for Lily picker
-- Copyright   : Copyright (C) 2023 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module Main (main) where

import Data.ByteString qualified as B
import Data.Text.Encoding (decodeUtf8Lenient, encodeUtf8)
import System.Exit
import System.IO

import Parser
import Printer

main :: IO ()
main = do
    contents <- decodeUtf8Lenient <$> B.getContents
    case parseLily "standard input" contents of
        Right output ->
            B.putStr . encodeUtf8 $! renderLily output
        Left err -> do
            B.hPutStr stderr . encodeUtf8 $! err
            exitWith $! ExitFailure 1
