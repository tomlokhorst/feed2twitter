-------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Copyright   : (c) 2009, Tom Lokhorst
-- License     : BSD3
--
-- Maintainer  : Tom Lokhorst <tom@lokhorst.eu>
-- Stability   : Experimental
--
-- Main module for `feed2twitter` executable.
-- Contains generic Atom entry and RSS item to Tweet map functions.
--
-------------------------------------------------------------------------------
module Main where

import Web.Feed2Twitter

import Text.Atom.Feed
import Text.RSS.Syntax

import Data.List
import System.Console.GetOpt
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  (cfg, rest) <- processArgs defaultConfig options header args
  if length rest /= 5
   then putStrLn $ usageInfo header options
   else do
        let cfg' = cfg { feedUrl   = rest !! 0
                       , username  = rest !! 1
                       , password  = rest !! 2
                       , cacheFile = rest !! 3
                       , cacheSize = read $ rest !! 4
                       }
        item2twitter cfg' item2tweet
  where
    header = "Usage: feed2twitter FEED-URL USERNAME PASSWORD CACHE-FILE"
              ++ " CACHE-SIZE [OPTIONS...], with the following options:"


item2tweet :: Either Entry RSSItem -> Tweet
item2tweet (Left entry) =
  if null (entryLinks entry)
  then trunc4tweet s
  else trunc4url s ++ linkHref (head (entryLinks entry))
  where
    title = showTextContent $ entryTitle entry
    s = case (entryAuthors entry, entrySummary entry) of
          ([], Nothing) -> title
          (as, Nothing) -> title ++ " by " ++ intercalate ", " (map personName as)
          ([], Just s ) -> title ++ ": " ++ showTextContent s
          (as, Just s ) -> title ++ " by " ++ intercalate ", " (map personName as)
                             ++ ": " ++ showTextContent s
item2tweet (Right item) = maybe (trunc4tweet s) (trunc4url s ++) (rssItemLink item)
  where
    -- Stupid optional fields...
    s = case (rssItemTitle item, rssItemAuthor item, rssItemDescription item) of
          (Nothing   , Nothing  , Nothing  ) -> ""
          (Just title, Nothing  , Nothing  ) -> title
          (Nothing   , Just auth, Nothing  ) -> auth ++ " posted."
          (Just title, Just auth, Nothing  ) -> title ++ " by " ++ auth
          (Nothing   , Nothing  , Just desc) -> desc
          (Just title, Nothing  , Just desc) -> (title ++ " " ++ desc)
          (Nothing   , Just auth, Just desc) -> auth ++ ": " ++ desc
          (Just title, Just auth, Just desc) -> title ++ " by " ++ auth ++ ": " ++ desc

showTextContent :: TextContent -> String
showTextContent (TextString s)  = s
showTextContent (HTMLString s)  = s -- Todo: make better
showTextContent (XHTMLString e) = show e -- Todo: also make better

defaultConfig :: Config
defaultConfig = Config
  { feedUrl = ""
  , username = ""
  , password = ""
  , cacheFile = ""
  , cacheSize = 0
  , debugMode = False
  }

options :: [OptDescr (Config -> Config)]
options =
  [ Option ['d'] ["debug-mode"] (NoArg (\c -> c { debugMode = True })) "Debug mode, send tweets to stdout."
  -- , Option ['V'] ["version"] (NoArg (\c -> c { showVersion = True })) "Show program version."
  ]

-- Seems like this function should already exist somewhere in a package.
processArgs :: a -> [OptDescr (a -> a)] -> String -> [String] -> IO (a, [String])
processArgs defaultConfig options header args =
  case getOpt Permute options args of
    (oargs, nonopts, []    ) -> return (foldl (flip ($)) defaultConfig oargs, nonopts)
    (_    , _      , errors) -> ioError $ userError $ (concat errors) ++ usageInfo header options


