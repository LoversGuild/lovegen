{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module contains various and thousands of orphan instances for Pandoc
-- types, so that Shake.Forward can cache them.

-- This module should not depend on any other module in the website generator, because of horrible compilation times.

module LoveGen.Orphans () where

import Data.Binary
import Data.Text qualified as T
import System.OsPath
import System.OsPath.Posix (PosixString)
import Text.DocTemplates qualified as DT
import Text.DocTemplates.Internal qualified as DT (Alignment, Border, Pipe, Variable)
import Text.Pandoc

instance Binary Alignment
instance Binary Block
instance Binary Caption
instance Binary Cell
instance Binary Citation
instance Binary CitationMode
instance Binary ColSpan
instance Binary ColWidth
instance Binary Text.Pandoc.Format
instance Binary Inline
instance Binary ListNumberDelim
instance Binary ListNumberStyle
instance Binary MathType
instance Binary Meta
instance Binary MetaValue
instance Binary QuoteType
instance Binary Row
instance Binary RowSpan
instance Binary RowHeadColumns
instance Binary TableBody
instance Binary TableHead
instance Binary TableFoot
instance Binary Pandoc

instance Binary (DT.Doc T.Text)
instance Binary DT.Alignment
instance Binary DT.Border
instance Binary DT.Pipe
instance Binary DT.Variable
instance Binary (Template T.Text)

instance Binary CiteMethod
instance Binary Extension
instance Binary Extensions
instance Binary ObfuscationMethod
instance Binary HTMLMathMethod
instance Binary ReferenceLocation
instance Binary TopLevelDivision
instance Binary TrackChanges
instance Binary WrapOption

-- instance Binary WriterOptions
-- deriving via (M.Map T.Text (DT.Val a)) instance (Binary a, Generic a) => Binary (DT.Context a)
-- instance (Binary a, Generic a) => Binary (DT.Val a)
   
instance Binary PosixString
instance Binary OsString
