-- |
-- Module      : LoveGen
-- Description : A static website generator support library
-- Copyright   : Copyright (C) 2022–2025 The Lovers' Guild
-- License     : GNU Affero General Public License version 3
-- Maintainer  : dev@loversguild.org
-- Stability   : experimental
-- Portability : GHC
--
-- “Generate Love, not war!”
--
-- LoveGen is a support library for writing static website generators.
-- This module exports all definitions in this package.
module LoveGen (
    module LoveGen.Config,
    module LoveGen.CommandLine,
    module LoveGen.Files,
    module LoveGen.Git,
    module LoveGen.Page,
    module LoveGen.Pandoc,
    module LoveGen.RoseTrie,
    module LoveGen.Site,
    module LoveGen.Url,
) where

import LoveGen.CommandLine
import LoveGen.Config
import LoveGen.Files
import LoveGen.Git
import LoveGen.Page
import LoveGen.Pandoc
import LoveGen.RoseTrie
import LoveGen.Site
import LoveGen.Url
