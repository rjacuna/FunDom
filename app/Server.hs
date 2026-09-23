{-# LANGUAGE OverloadedStrings #-}
-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | The web server. It parses the query string and hands it to the library;
-- there is no state and no mathematics here.
module Main (main) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Data.IORef
import Modular.CP (Summary, scanAll)
import Render.Page (renderPage, renderSvgOnly)
import System.Environment (getArgs)
import System.IO
import Web.Group (resolve, csgDirDefault)
import Web.Params (parseParams)

main :: IO ()
main = do
  args <- getArgs
  let (port, dir) = case args of
        [p]    -> (read p, csgDirDefault)
        [p, d] -> (read p, d)
        _      -> (8080, csgDirDefault)
  hSetBuffering stdout LineBuffering
  putStrLn ("fundom-server: http://localhost:" ++ show port ++ "/   tables: " ++ dir)
  -- the tables' summaries, read once, the first time mode 2 asks
  cache <- newIORef Nothing
  let loader = do
        c <- readIORef cache
        case c of
          Just sms -> return sms
          Nothing  -> do
            sms <- scanAll dir
            writeIORef cache (Just sms)
            return sms
  run port (app dir loader)

app :: FilePath -> IO [Summary] -> Application
app dir loader req respond = case pathInfo req of
  []      -> resolve dir loader params >>= \cx -> respond (page "text/html; charset=utf-8" (renderPage cx))
  ["svg"] -> resolve dir loader params >>= \cx -> respond (page "image/svg+xml; charset=utf-8" (renderSvgOnly cx))
  _       -> respond (responseLBS status404 [("Content-Type", "text/plain")] "not found")
  where
    query  = [ (dec k, maybe "" dec v) | (k, v) <- queryString req ]
    params = parseParams query
    dec = T.unpack . TE.decodeUtf8With TE.lenientDecode
    page ct body = responseBuilder status200
      [("Content-Type", ct), ("Cache-Control", "no-store")] body
