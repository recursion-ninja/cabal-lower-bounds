module GHC.Distribution.Table
    ( -- * Data-types
      -- ** Tabular Structures
      Digest
    , Header
      -- ** Indices
    , Ordinal ()
    , PackageRank ()
    , VersionRank (..)
      -- ** Datum
    , VersionPart ()
    , partNumber
      -- * Pre-computed Lookup Table
    , packageIndex
    , versionTable
    ) where

import GHC.Distribution.Table.Load
import GHC.Distribution.Table.Type
