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
import           Data.Set
import qualified Data.Text                as T
import qualified Heist.Interpreted        as I
import           Models.Utils
import           Text.HTML.SanitizeXSS
import           Text.Pandoc
import           Text.Pandoc.Shared       (tabFilter)
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

readDoc :: String -> Pandoc
readDoc = readMarkdown parserOptions

parserOptions :: ReaderOptions
parserOptions = let d = def
                    ext = insert Ext_literate_haskell (readerExtensions d)
                in
                d { readerExtensions = ext }

writeDoc :: Pandoc -> String
writeDoc = UTF8.encodeString . writeHtmlString writerOptions

writerOptions :: WriterOptions
writerOptions = def { writerHighlight = True
                    , writerHTMLMathMethod = googleApiMathMethod
                    }

googleApiMathMethod :: HTMLMathMethod
googleApiMathMethod = WebTeX "http://chart.apis.google.com/chart?cht=tx&chl="
