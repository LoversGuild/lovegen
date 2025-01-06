-- |
-- Module      : LoveGen.CommandLine
-- Description : Command-line argument parser for LoveGen
-- Copyright   : Copyright (C) 2022–2025 The Lovers' Guild
-- License     : GNU Affero General Public License version 3
-- Maintainer  : dev@loversguild.org
-- Stability   : experimental
-- Portability : GHC
--
-- LoveGen's command-line parser
module LoveGen.CommandLine (
    CmdLineOptions (..),
    parseCmdLine,
) where

import Options.Applicative

-- Define a data structure for the command line arguments
data CmdLineOptions = CmdLineOptions
    { directory :: FilePath
    }
    deriving stock (Show)

-- | The core of the command-line options parser
optionsParser :: Parser CmdLineOptions
optionsParser = do
    directory <- argument str (metavar "DIRECTORY" <> help "The root directory of the website sources")
    pure CmdLineOptions {..}

-- | Parse command-line options
parseCmdLine :: IO CmdLineOptions
parseCmdLine = execParser optsInfo
  where
    optsInfo =
        info
            (optionsParser <**> helper)
            ( fullDesc
                <> progDesc "Generate a static website"
                <> header "lovegen generates static websites from Markdown files using Pandoc."
            )
