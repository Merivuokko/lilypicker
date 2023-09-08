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
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8Lenient)

import Parser
import Printer

main :: IO ()
main = B.getContents >>= putStrLn . T.unpack . either id printLily . parseLily "standard input" . decodeUtf8Lenient
