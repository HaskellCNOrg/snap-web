{-# LANGUAGE OverloadedStrings #-}

module Views.PaginationSplices where

import qualified Data.Text                 as T
import           Snap.Snaplet.Environments
import           Snap.Snaplet.Heist
import           Text.Templating.Heist
import qualified Text.XmlHtml              as X

import           Application
import           Models.Utils


-- | Select items for particular page base on Page Size, Current Page
--
paginationHandler :: (Eq b, Integral a, Show a)
                     => Double   -- ^ Page Size
                     -> a        -- ^ Current Page
                     -> [b]      -- ^ Total items
                     -> AppHandler ([b], Splice AppHandler) -- ^ items for current page and page splice
paginationHandler s cp xs = do
  pageSize <- getPageSize
  let total          = length xs
      sDouble        = fromIntegral pageSize
      pageCount'     = ceiling $ fromIntegral total / sDouble
      cp'            = fixCurrentPage cp pageCount'
      pageNumberList = [1..pageCount']
      pageItems      = sliceForPage cp' pageSize xs
      pageSplice     = paginationSplice cp' pageNumberList
  return (pageItems, pageSplice)

getPageSize :: AppHandler Int
getPageSize = lookupConfigDefault "pagesize" 20

----------------------------------------------------------------------------

-- | Splice for display pagination elements.
--
paginationSplice :: (Show a, Integral a)
                    => a                 -- ^ Current Page
                    -> [a]               -- ^ Total size
                    -> Splice AppHandler
paginationSplice cp xs = return [paginationNode cp xs]

-- | Generate HTML nodes for topic pagination.
--   Elements created here because of not sure how to set up "active" class.
--
paginationNode :: (Show a, Integral a)
               => a             -- ^ Current Page
               -> [a]           -- ^ Page Number List
               -> X.Node        -- ^ HTML Nodes for page numbers
paginationNode _ [] = X.Comment "insufficient data for pagination"
paginationNode i xs =
    let cp = sToText i
        doNode = X.Element "ul" [] lis
        lis = map (f . sToText) xs
        f n
          | cp == n   = X.Element "li" [("class","active")] [a n]
          | otherwise = X.Element "li" [] [a n]
        a n
          | cp == n   = X.Element "a" [] [X.TextNode n]
          | otherwise = X.Element "a" [("href", topicPageLink n)] [X.TextNode n]
    in doNode


topicPageLink :: T.Text -> T.Text
topicPageLink = T.append topicHref

topicHref :: T.Text
topicHref = "/?pagenum="


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
