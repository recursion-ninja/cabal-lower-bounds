{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}


module GHC.DistributionTable
  ( -- * Data-types
    -- ** Tabular Structures
    Header
  , Digest
    -- ** Indices
  , Ordinal()
  , PackageRank()
  , VersionRank(..)
    -- ** Datum
  , VersionPart()
  , partNumber
    -- * Pre-computed Lookup Table
  , loadHeader
  ) where

import GHC.DistributionTable.Type
import GHC.DistributionTable.Load
