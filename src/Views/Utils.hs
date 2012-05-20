{-# LANGUAGE OverloadedStrings #-}

{-

utils.

-}

module Views.Utils where

----------------------------------------------------------------

import qualified Data.Text as T
import           Data.Time
import           System.Locale


------------------------------------------------------------------------------

-- | UTCTime to Text
-- 
formatUTCTime :: UTCTime -> T.Text
formatUTCTime = T.pack . formatTime defaultTimeLocale "%F %H:%M"

-- | per Timezone format
formatUTCTimePerTZ :: TimeZone -> UTCTime -> T.Text
formatUTCTimePerTZ tz tm = T.pack . formatTime defaultTimeLocale "%F %H:%M" $ utcToLocalTime tz tm
