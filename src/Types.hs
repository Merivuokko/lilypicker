-- |
-- Module      : Types
-- Description : LilyPicker data type
-- Copyright   : Copyright (C) 2023 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module Types (
    LocatedText (..),
    Lily (..),
    emptyLily,
    Part (..),
    PartName,
    PartMap,
) where

import Data.DList (DList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM (empty)
import Data.Text (Text)
import Text.Megaparsec (SourcePos)

-- | LocatedText holds a strict `Text` value accompanied by its source location.
data LocatedText = LocatedText
    { -- | Source location for this value
      pos :: SourcePos,
      -- | Contents
      value :: Text
    }
    deriving stock (Eq, Show)

-- | Part names are text
type PartName = Text

-- | Parts contain music and the function they will be wrapped in
data Part = Part
    { -- | Name of this part
      name :: LocatedText,
      -- | A LilyPond music function that si used to wrap the contents of this part
      function :: Maybe Text,
      -- | Part contents
      contents :: DList LocatedText
    }
    deriving stock (Eq, Show)

-- | A type alias for mappings from part names to part contents
type PartMap = HashMap PartName Part

-- | Lily contains all data gathered from a Lily picker file
data Lily = Lily
    { -- | Preamble text
      preamble :: DList LocatedText,
      -- | Epilogue text
      epilogue :: DList LocatedText,
      -- | Parts
      parts :: HashMap PartName Part
    }
    deriving stock (Eq, Show)

emptyLily :: Lily
emptyLily =
    Lily
        { preamble = mempty,
          epilogue = mempty,
          parts = HM.empty
        }
