-------------------------------------------------------------------------------
-- |
-- Module      : Web.Feed2Twitter.Twitter
-- Copyright   : (c) 2009, Tom Lokhorst
-- License     : BSD3
--
-- Maintainer  : Tom Lokhorst <tom@lokhorst.eu>
-- Stability   : Experimental
--
-- Twitter functions, build on hs-twitter package.
--
-------------------------------------------------------------------------------
module Web.Feed2Twitter.Twitter where

import Web.Twitter.Fetch
import Web.Twitter.Monad

import System.Environment

tweet :: String -> String -> String -> IO ()
tweet username password message = do
  runTM (AuthUser username password)
    $ postMethod
    $ restCall "update.json" (arg "status" message [])
  return ()


