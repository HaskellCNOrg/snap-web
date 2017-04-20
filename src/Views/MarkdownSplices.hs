{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

-- | transform any markdown content to html
--
--
module Views.MarkdownSplices
       ( markdownToHtmlSplice
       , markdownToHtmlBS
       , markdownToHtmlText
       ) where

import qualified Codec.Binary.UTF8.String as UTF8
import           Control.Monad.Trans
import qualified Data.ByteString.Char8    as BS
import qualified Data.Set as Set
import qualified Data.Text                as T
import qualified Heist.Interpreted        as I
import           Models.Utils
import           Text.HTML.SanitizeXSS
import           Text.Pandoc
import           Text.Pandoc.Shared       (tabFilter)
import           Text.Pandoc.Error
import qualified Text.XmlHtml             as X

----------------------------------------------------------------------

markdownToHtmlSplice :: MonadIO m => T.Text -> I.Splice m
markdownToHtmlSplice markup =
    either throwError toDoc $ X.parseHTML "" $ markdownToHtmlBS markup
    where throwError e = return [X.TextNode $ T.pack ("Error parsing markdown output: " ++ e)]
                         --MAYBE:ERROR STYLE
          toDoc = return . X.docContent

xss :: BS.ByteString -> T.Text
xss = sanitizeBalance . bsToText

-- | Convert tabs to spaces and filter out DOS line endings.
tabFilter4 :: String -> String
tabFilter4 = tabFilter 4

-- | Convert markdown doc to @ByteString@
--
markdownToHtmlBS :: T.Text -> BS.ByteString
markdownToHtmlBS = textToBS . markdownToHtmlText

-- | Convert markdown doc to @Text@
--
markdownToHtmlText :: T.Text -> T.Text
markdownToHtmlText =  xss . BS.pack . writeDoc . readDoc . tabFilter4 . T.unpack

readDoc :: String ->  Either PandocError Pandoc
readDoc = readMarkdown parserOptions

parserOptions :: ReaderOptions
parserOptions = let d = def
                    ext = Set.union (readerExtensions d) hcnMarkdownExtensions
                in
                d { readerExtensions = ext }

writeDoc ::  Either PandocError Pandoc -> String
writeDoc (Right doc) = UTF8.encodeString $ writeHtmlString writerOptions doc
writeDoc (Left err) = show err

writerOptions :: WriterOptions
writerOptions = def { writerHighlight = True
                    , writerHTMLMathMethod = googleApiMathMethod
                    }

googleApiMathMethod :: HTMLMathMethod
googleApiMathMethod = WebTeX "http://chart.apis.google.com/chart?cht=tx&chl="

hcnMarkdownExtensions :: Set.Set Extension
hcnMarkdownExtensions = Set.fromList
  [ Ext_pipe_tables
  , Ext_raw_html
  , Ext_tex_math_single_backslash
  , Ext_fenced_code_blocks
  , Ext_fenced_code_attributes
  , Ext_auto_identifiers
  , Ext_ascii_identifiers
  , Ext_backtick_code_blocks
  , Ext_autolink_bare_uris
  , Ext_intraword_underscores
  , Ext_strikeout
  , Ext_hard_line_breaks
  , Ext_lists_without_preceding_blankline
  , Ext_literate_haskell
  ]