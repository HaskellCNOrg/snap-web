{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

-- | transform any markdown content to html
--
--
module Views.MarkdownSplices
       ( markdownToHtmlSplice ) where

import qualified Codec.Binary.UTF8.String               as UTF8
import           Control.Monad.CatchIO
import           Control.Monad.Trans
import qualified Data.ByteString.Char8                  as BS
import qualified Data.Text                              as T
import           Text.Pandoc
import           Text.Pandoc.Shared                     (tabFilter)
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Markdown
import qualified Text.XmlHtml                           as X


----------------------------------------------------------------------

markdownToHtmlSplice :: MonadIO m => T.Text -> Splice m
markdownToHtmlSplice markup =
    either throwError toDoc $ X.parseHTML "" $ markdownToHtmlString markup
    where throwError e = return [X.TextNode $ T.pack ("Error parsing markdown output: " ++ e)]
          toDoc = return . X.docContent

------------------------------------------------------------------------------

-- | Convert tabs to spaces and filter out DOS line endings.
tabFilter4 :: String -> String
tabFilter4 = tabFilter 4

markdownToHtmlString :: T.Text -> BS.ByteString
markdownToHtmlString = BS.pack . writeDoc . readDoc . tabFilter4 . T.unpack

readDoc :: String -> Pandoc
readDoc = readMarkdown (defaultParserState { stateLiterateHaskell = True })

writeDoc :: Pandoc -> String
writeDoc = UTF8.encodeString . writeHtmlString defaultWriterOptions

