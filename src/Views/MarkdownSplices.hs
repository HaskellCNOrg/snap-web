{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

-- | transform any markdown content to html
-- 
-- 
module Views.MarkdownSplices 
       ( markdownToHtmlSplice ) where

import           Text.Pandoc.Shared (tabFilter)
import           Control.Monad.Trans
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import           Text.Templating.Heist
import           Text.Pandoc
import           Text.Templating.Heist.Splices.Markdown
import qualified Text.XmlHtml as X
import           Control.Monad.CatchIO


----------------------------------------------------------------------

markdownToHtmlSplice :: MonadIO m => T.Text -> Splice m
markdownToHtmlSplice markup = 
    either throwError toDoc $ X.parseHTML "" $ markdownToHtmlString markup
    where throwError e = throw $ MarkdownException
                         $ BS.pack ("Error parsing markdown output: " ++ e)
          toDoc = return . X.docContent
      
------------------------------------------------------------------------------
    
-- | Convert tabs to spaces and filter out DOS line endings.    
tabFilter4 :: String -> String
tabFilter4 = tabFilter 4    

markdownToHtmlString :: T.Text -> BS.ByteString
markdownToHtmlString = BS.pack . writeDoc . readDoc . tabFilter4 . T.unpack

readDoc :: String -> Pandoc
readDoc = readMarkdown defaultParserState

writeDoc :: Pandoc -> String
writeDoc = writeHtmlString defaultWriterOptions

