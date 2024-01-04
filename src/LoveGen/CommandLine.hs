-- |
-- Module      : LoveGen.CommandLine
-- Description : Command-line argument parser for LoveGen
-- Copyright   : Copyright (C) 2023-2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
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
