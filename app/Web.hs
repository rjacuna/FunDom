{-# LANGUAGE ForeignFunctionInterface #-}
-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | The wasm reactor exports: the same 'renderPage' the server calls, with
-- the page's inputs handed in as strings. JavaScript owns the table data
-- (a JSON export) and passes one record and one list of summaries per
-- request in the compact formats of "Modular.CP".
module Web (render, svg, level) where

import GHC.Wasm.Prim
import Modular.CP (parseCompact, parseSummaries, parseOptions)
import Render.Page
import Web.Context (Context)
import Web.Group (contextFrom)
import Web.Params

-- `sync`: the export returns a string, not a Promise.
foreign export javascript "render sync" render :: JSString -> JSString -> JSString -> IO JSString
foreign export javascript "svg sync"    svg    :: JSString -> JSString -> JSString -> IO JSString
foreign export javascript "level sync"  level  :: JSString -> JSString -> IO JSString

context :: JSString -> JSString -> JSString -> Context
context q r s = contextFrom (parseParams (parseQuery (fromJSString q))) rec (parseSummaries txt, parseOptions txt)
  where
    txt = fromJSString s
    rec = case fromJSString r of
      "" -> Left "choose a group"
      t  -> parseCompact t

render :: JSString -> JSString -> JSString -> IO JSString
render q r s = pure (toJSString (renderPage (context q r s)))

svg :: JSString -> JSString -> JSString -> IO JSString
svg q r s = pure (toJSString (renderSvgOnly (context q r s)))

level :: JSString -> JSString -> IO JSString
level n s = pure (toJSString (renderLevel (read (fromJSString n)) (parseSummaries (fromJSString s))))
