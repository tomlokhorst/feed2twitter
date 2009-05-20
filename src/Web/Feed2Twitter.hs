-------------------------------------------------------------------------------
-- |
-- Module      : Web.Feed2Twitter
-- Copyright   : (c) 2009, Tom Lokhorst
-- License     : BSD3
--
-- Maintainer  : Tom Lokhorst <tom@lokhorst.eu>
-- Stability   : Experimental
--
-- This module exposes several functions and data types to send feeds to
-- Twitter.
--
-------------------------------------------------------------------------------
module Web.Feed2Twitter
  (
  -- * Types
    Config (..)
  , Tweet
  , GUID

  -- * Convenient user functions
  , atom2twitter
  , rss2twitter
  , item2twitter

  -- * Main function
  , feed2twitter

  -- * Util functions
  , trunc4tweet
  , trunc4url
  ) where

import Web.Feed2Twitter.Twitter

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Network.Curl.Download
import Text.HTML.TagSoup
import Text.Atom.Feed
import Text.Feed.Import
import Text.RSS.Syntax
import Text.Feed.Types (Feed (AtomFeed, RSSFeed))
import qualified Text.Feed.Types as Feed

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord (comparing)
import System.IO

-- | Configuration data for the `feed2twitter` function.
data Config = Config
  { feedUrl   :: String
  , username  :: String
  , password  :: String
  , cacheFile :: FilePath
  , cacheSize :: Int
  , debugMode :: Bool
  }
  deriving Show

-- | Represents a single tweet.
-- In principle this should be <= 140 characters, however, because Twitter uses
-- a url-shortener, a tweet containing a url may exeed this limit.
type Tweet = String

-- | A unique identifier for a Tweet.
-- Values of this type are stored in the cache file to make sure no duplicate
-- messages are sent to Twitter.
type GUID  = String

-- | Sends an Atom feed to Twitter by using user-provided mapping function
-- for individual entries.
--
-- This is a specialized version of `item2twitter`.
atom2twitter :: Config -> (Entry -> Tweet) -> IO ()
atom2twitter cfg f = item2twitter cfg g
  where
    err = "Web.Feed2Twitter.atom2twitter: "
    g (Left e)  = f e
    g (Right _) = error $ err ++ "Item not an Atom entry"

-- | Sends a RSS feed to Twitter by using user-provided mapping function
-- for individual items.
--
-- This is a specialized version of `item2twitter`.
rss2twitter :: Config -> (RSSItem -> Tweet) -> IO ()
rss2twitter cfg f = item2twitter cfg g
  where
    err = "Web.Feed2Twitter.rss2twitter: "
    g (Left _)  = error $ err ++ "Item not a RSS item"
    g (Right i) = f i

-- | Sends feed items to Twitter by using user-provided mapping function
-- for individual items.
-- 
-- Defined in terms of `feed2twitter`.
item2twitter :: Config -> (Either Entry RSSItem -> Tweet) -> IO ()
item2twitter cfg f = feed2twitter cfg g
  where
    err = "Web.Feed2Twitter.items2twitter: "

    -- Feeds are asumed to be in reverse chronological order.
    -- Tweets are posted in chronological order.
    g (AtomFeed af) = map atom . reverse . feedEntries $ af
    g (RSSFeed rf)  = map rss  . reverse . rssItems . rssChannel $ rf
    g _             = error $ err ++ "Feed not an Atom or RSS feed"

    atom entry      = (entryId entry, f (Left entry))
    rss ri          = (guid, f (Right ri))
     where
       guid = maybe (filter (/='\n') $ show ri) rssGuidValue (rssItemGuid ri)

-- | Function that does all the heavy lifting:
--
-- - Downloads the feed provided in the config value.
--
-- - Calls the provided user-function to map feed to a list of guids and tweets.
--
-- - Filters out already posted tweets using guid cache.
--
-- - Sends each tweet to Twitter and writes its guid to cache file.
--
-- - Truncate cache file to size provided in config value.
-- 
-- User-function is responsible for generating guids for tweets and the order
-- of tweets.
feed2twitter :: Config -> (Feed.Feed -> [(GUID, Tweet)]) -> IO ()
feed2twitter cfg f = do
    feed' <- openAsFeed (feedUrl cfg)
    case feed' of
      Left s     -> error $ err ++ s
      Right feed -> do
        let tweets' = f feed
        touchFile (cacheFile cfg)
        cfc <- BS.readFile (cacheFile cfg)
        let guids = lines (BSC.unpack cfc)
        let tweets = filter (pred guids) tweets'
        mapM_ tweetAndCache tweets
        when (not $ null tweets) cleanupCache
  where
    err = "Web.Feed2Twitter.feed2twitter: "

    -- Quick hack to create the file if it didn't exist
    touchFile f = openFile f ReadWriteMode >>= hClose

    pred guids (g, t) = not (g `elem` guids || t == "")
    cleanupCache = do
      guids' <- BS.readFile (cacheFile cfg)
      let guids = BSC.unlines . reverse . take (cacheSize cfg) . reverse
                   . BSC.lines $ guids'
      BS.writeFile (cacheFile cfg) guids

    tweetAndCache (guid, t) =
      if debugMode cfg
       then putStrLn ("tweet: " ++ t)
       else do
            tweet (username cfg) (password cfg) t

            -- Immediately write to cache after tweet, in case process crashes
            BS.appendFile (cacheFile cfg) (BSC.pack $ guid ++ "\n")

-- | Truncates a string to 140 characters for a tweet.
--
-- When input is longer than 140 characters, puts an ellipsis at the end.
trunc4tweet :: String -> String
trunc4tweet s = if length s <= 140
                then s
                else take 139 s ++ "\8230"

-- | Truncates a string to 120 characters, leaving room for a space and a url.
-- Due to Twitter using a url-shortener, urls are assumed to max 20 characters.
--
-- When input is shorter than 120 characters, returns it with a space,
-- otherwise truncates and puts an ellipsis and space at the end.
trunc4url :: String -> String
trunc4url s = if length s <= 119
              then s ++ " "
              else take 118 s ++ "\8230 "

