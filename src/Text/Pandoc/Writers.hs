{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{- |
   Module      : Text.Pandoc
   Copyright   : Copyright (C) 2006-2023 John MacFarlane
   License     : GNU GPL, version 2 or above

   Maintainer  : John MacFarlane <jgm@berkeley.edu>
   Stability   : alpha
   Portability : portable

This helper module exports all writers functions.
-}
module Text.Pandoc.Writers
  (
    -- * Writers: converting /from/ Pandoc format
      Writer(..)
    , writers
    , writeCommonMark
    , writeHtml4
    , writeHtml4String
    , writeHtml5
    , writeHtml5String
    , writeJSON
    , writeMarkdown
    , writeNative
    , writePlain
    , getWriter
    ) where

import Control.Monad.Except (throwError)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Text.Pandoc.Class
import Text.Pandoc.Definition
import qualified Text.Pandoc.Format as Format
import Text.Pandoc.Options
import qualified Text.Pandoc.UTF8 as UTF8
import Text.Pandoc.Error
import Text.Pandoc.Writers.CommonMark
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Writers.Markdown
import Text.Pandoc.Writers.Native

data Writer m = TextWriter (WriterOptions -> Pandoc -> m Text)
              | ByteStringWriter (WriterOptions -> Pandoc -> m BL.ByteString)

-- | Association list of formats and writers.
writers :: PandocMonad m => [ (Text, Writer m) ]
writers = [
   ("native"       , TextWriter writeNative)
  ,("json"         , TextWriter writeJSON)
  ,("html"         , TextWriter writeHtml5String)
  ,("html4"        , TextWriter writeHtml4String)
  ,("html5"        , TextWriter writeHtml5String)
  ,("markdown"     , TextWriter writeMarkdown)
  ,("markdown_strict" , TextWriter writeMarkdown)
  ,("markdown_phpextra" , TextWriter writeMarkdown)
  ,("markdown_github" , TextWriter writeMarkdown)
  ,("markdown_mmd" , TextWriter writeMarkdown)
  ,("plain"        , TextWriter writePlain)
  ,("commonmark"   , TextWriter writeCommonMark)
  ,("commonmark_x" , TextWriter writeCommonMark)
  ,("gfm"          , TextWriter writeCommonMark)
  ]

-- | Retrieve writer, extensions based on formatSpec (format+extensions).
getWriter :: PandocMonad m => Format.FlavoredFormat -> m (Writer m, Extensions)
getWriter flvrd = do
  let writerName = Format.formatName flvrd
  case lookup writerName writers of
    Nothing  -> throwError $ PandocUnknownWriterError writerName
    Just  w  -> (w,) <$>
      Format.applyExtensionsDiff (Format.getExtensionsConfig writerName) flvrd

writeJSON :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeJSON _ = return . UTF8.toText . BL.toStrict . encode
