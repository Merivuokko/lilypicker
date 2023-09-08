-- |
-- Module      : Printer
-- Description : LilyPond printer
-- Copyright   : Copyright (C) 2023 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module Printer (
    printLily,
) where

import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TL

import Types

-- | Render a Lily data structure into pure text
printLily :: Lily -> T.Text
printLily lily =
    TL.toStrict . TL.toLazyText $!
        lily.preamble
            <> "\n"
            <> HM.foldlWithKey' (\acc key part -> printPart key part <> acc) "" lily.parts
            <> lily.epilogue
            <> "\n"
  where
    printPart :: PartName -> Part -> TL.Builder
    printPart name part = TL.fromText name <> " =" <> (maybe "" ((" " <>) . TL.fromText) part.function) <> " {\n" <> part.contents <> "}\n\n"
