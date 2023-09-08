-- |
-- Module      : Types
-- Description : LilyPicker data type
-- Copyright   : Copyright (C) 2023 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module Types (
    Lily (..),
    emptyLily,
    Part (..),
    PartName,
    PartMap,
) where

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM (empty)
import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)

-- | Part names are text
type PartName = Text

-- | Parts contain music and the function they will be wrapped in
data Part = Part
    { -- | Part contents
      contents :: Builder,
      -- | Name and parameters of the music function that is to wrap this part's contents
      function :: Maybe Text
    }
    deriving stock (Eq, Show)

-- | A type alias for mappings from part names to part contents
type PartMap = HashMap PartName Part

-- | Lily contains all data gathered from a Lily picker file
data Lily = Lily
    { -- | Preamble text
      preamble :: Builder,
      -- | Epilogue text
      epilogue :: Builder,
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
