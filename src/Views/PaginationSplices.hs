{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Views.PaginationSplices where

import           Snap.Snaplet.Heist
import           Text.Templating.Heist
import qualified Data.Text as T
import qualified Text.XmlHtml as X

import           Application
import           Models.Utils

data Integral a => Pagination a = Pagination 
                    { totalSize :: a
                    , currentPage :: a
                    , pagination :: [a]
                    }

type PageNum = Double

pageSize :: PageNum
pageSize = 3

pageSplices :: [(T.Text, Splice AppHandler)]
pageSplices = [ ("pagination", paginationSplice 2)
                ]


----------------------------------------------------------------------------

-- | Splice for display pagination elements.
-- 
paginationSplice :: Integral a => a -> Splice AppHandler
paginationSplice a = return [paginationNode 1 4]

-- | Generate HTML nodes for pagination.
-- 
paginationNode :: PageNum        -- ^ Page Number
               -> PageNum        -- ^ Total Size
               -> X.Node         -- ^ HTML Nodes
paginationNode _ 0 = X.Comment "insufficient data for pagination"
paginationNode 0 t = paginationNode 1 t
paginationNode p t = X.Element "ul" [] lis
                     where lis = map f $ map sToText (genPaginationList p t)
                           f n = X.Element "li" [] [ a n ]
                           a n = X.Element "a" [] [(X.TextNode n)]

genPaginationList :: (Integral b)
                     => PageNum        -- ^ Page Number
                     -> PageNum        -- ^ Total Size
                     -> [b]            -- ^ Pagination
genPaginationList _ 0 = []
genPaginationList 0 y = genPaginationList 1 y
genPaginationList x y = [1..n]
                        where n = ceiling $ y / pageSize
                     
forceNonEmpty :: Integral a => Maybe a -> a
forceNonEmpty Nothing = 1
forceNonEmpty (Just 0) = 1
forceNonEmpty (Just a) = a
