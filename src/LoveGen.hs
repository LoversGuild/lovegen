-- |
-- Module      : LoveGen
-- Description : A static website generator support library
-- Copyright   : Copyright (C) 2023-2024 The Lovers' Guild
-- License     : AGPL-3
-- Maintainer  : dev@loversguild.org
-- Stability   : experimental
-- Portability : GHC
--
-- “Make love, not war!”
--
-- LoveGen is a support library for writing static website generators.
-- This module exports all definitions in this package.
module LoveGen (
    module LoveGen.Files,
    module LoveGen.Git,
    module LoveGen.Pandoc,
) where

import LoveGen.Files
import LoveGen.Git
import LoveGen.Pandoc
