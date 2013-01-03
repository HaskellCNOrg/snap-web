{-# LANGUAGE OverloadedStrings #-}

module Views.PaginationSplices where

import qualified Data.ByteString           as BS
import qualified Data.Text                 as T
import           Snap
import           Snap.Snaplet.Environments
import qualified Heist.Interpreted as I
import qualified Text.XmlHtml              as X

import           Application
import           Models.Utils


-- | Select items for particular page base on Page Size, Current Page
--
paginationHandler :: (Eq b, Integral a, Show a)
                     => a        -- ^ Current Page
                     -> [b]      -- ^ Total items
                     -> AppHandler (Int, [b], I.Splice AppHandler) -- ^ items for current page and page splice
paginationHandler cp xs = do
  pageSize <- getPageSize
  req <- getRequest
  let uri = requestURL (rqURI req)
--  liftIO $ print $
  let total          = length xs
      sDouble        = fromIntegral pageSize
      pageCount'     = ceiling $ fromIntegral total / sDouble
      cp'            = fixCurrentPage cp pageCount'
      pageNumberList = [1..pageCount']
      pageItems      = sliceForPage cp' pageSize xs
      pageSplice     = return [paginationNode cp' pageNumberList (urlGen uri)]
      startIndex     = 1 + pageSize * fromIntegral (cp' - 1)
  return (startIndex, pageItems, pageSplice)

getPageSize :: AppHandler Int
getPageSize = lookupConfigDefault "pagesize" 20

-- | Request URL could be "/", "/tags/xxx", "/?pagenum=2", etc.
--   Fetch the URI regardless of parameter via '?'.
--
requestURL :: BS.ByteString -> T.Text
requestURL = T.takeWhile (/= '?') . bsToText

----------------------------------------------------------------------------

-- | Generate HTML nodes for topic pagination.
--   Elements created here because of not sure how to set up "active" class.
--
paginationNode :: (Show a, Integral a)
               => a                   -- ^ Current Page
               -> [a]                 -- ^ Page Number List
               -> (T.Text -> T.Text)  -- ^ pagination url generator
               -> X.Node        -- ^ HTML Nodes for page numbers
paginationNode _ [] _ = X.Comment "insufficient data for pagination"
paginationNode i xs gen =
    let cp = sToText i
        doNode = X.Element "ul" [] lis
        lis = map (f . sToText) xs
        f n
          | cp == n   = X.Element "li" [("class","active")] [a n]
          | otherwise = X.Element "li" [] [a n]
        a n
          | cp == n   = X.Element "a" [] [X.TextNode n]
          | otherwise = X.Element "a" [("href", gen n)] [X.TextNode n]
    in doNode


-- | Create URL generator base on given root url
--
urlGen :: T.Text -> T.Text -> T.Text
urlGen t = T.append (t `T.append` topicHref)

topicHref :: T.Text
topicHref = "?pagenum="


---------------------------------------------------------------

-- | slice for page.
--   Fetching items from whole collection for particular page.
sliceForPage :: (Eq b, Integral a)
                => a              -- ^ current page
                -> Int            -- ^ page size
                -> [b]            -- ^ all items
                -> [b]            -- ^ selected items for current page.
sliceForPage _ _ []  = []
sliceForPage 0 s xs  = sliceForPage 1 s xs
sliceForPage cp size xs = take size $ drop (drops cp) xs
                     where drops = (* size) . flip (-) 1 . fromIntegral


---------------------------------------------------------------


-- | Fixing invalide page number.
--   e.g. total page number is 2 but given current page 3, show the last page (2 here) instead.
--
fixCurrentPage :: Integral a
                  => a  -- ^ Current Page
                  -> a  -- ^ How many page numbers in total
                  -> a  -- ^ A valid page number
fixCurrentPage cp total = if cp > total then total else cp
