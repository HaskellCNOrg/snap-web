{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.PaginationSplices where

import           Snap.Snaplet.Heist
import           Text.Templating.Heist
import qualified Data.Text as T
import qualified Text.XmlHtml as X

import           Application
import           Models.Utils


pageSize :: Double
pageSize = 2


----------------------------------------------------------------------------

-- | Splice for display pagination elements.
-- 
paginationSplice :: Integral a 
                    => a                 -- ^ Current Page
                    -> Double            -- ^ Total size
                    -> Splice AppHandler
paginationSplice cp total = return $ [paginationNode cp (genPaginationList total)]

-- | Generate HTML nodes for topic pagination.
--   Elements created here because of not sure how to set up "active" class.
-- 
paginationNode :: Integral a 
               => a             -- ^ Current Page
               -> [a]           -- ^ Page Number List
               -> X.Node        -- ^ HTML Nodes for page numbers
paginationNode _ [] = X.Comment "insufficient data for pagination"
paginationNode i xs = 
    let cp = sToText $ withFixCurrentPage2 i xs
        doNode = X.Element "ul" [] lis
        lis = map (f . sToText) xs
        f n 
          | cp == n   = X.Element "li" [("class","active")] [a n]
          | otherwise = X.Element "li" [] [a n]
        a n 
          | cp == n   = X.Element "a" [] [X.TextNode n]
          | otherwise = X.Element "a" [("href", topicPageLink n)] [(X.TextNode n)]
    in doNode


topicPageLink :: T.Text -> T.Text
topicPageLink = T.append topicHref

topicHref :: T.Text
topicHref = "/?pagenum="

-- | Generate Page Number List base on total size and predefined page size.
-- 
genPaginationList :: (Integral b)
                     => Double       -- ^ Total Size
                     -> [b]          -- ^ Page Number List
genPaginationList 0 = []
genPaginationList y = [1..n]
                      where n = pageCount y

pageCount :: Integral b => Double -> b
pageCount y = ceiling $ y / pageSize

-- | slice for page.
--   Fetch items for particular page.
sliceForPage :: (Eq b, Integral a)
                => a
                -> [b]
                -> [b]
sliceForPage _ []  = []
sliceForPage cp xs = doSlice (withFixCurrentPage cp xs)
                     where doSlice p = take takes $ drop (drops p) xs
                           takes     = fromIntegral (truncate pageSize)
                           drops p   = fromIntegral ((truncate pageSize) * (p - 1))


---------------------------------------------------------------


-- | Do pagination with fixing invalide page number.
-- 
withFixCurrentPage :: Integral a
                      => a         -- ^ page number
                      -> [b]       -- ^ collection
                      -> a         -- ^ corrected page number
withFixCurrentPage cp xs = let size = pageCount . fromIntegral $ length xs
                           in if cp > size then size else cp

withFixCurrentPage2 :: Integral a => a -> [b] -> a
withFixCurrentPage2 cp xs = let size = fromIntegral $ length xs
                            in if cp > size then size else cp

