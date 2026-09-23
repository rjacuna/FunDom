{-# LANGUAGE ForeignFunctionInterface #-}
-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | The wasm reactor exports: the same 'renderPage' the server calls, with
-- the page's inputs handed in as strings. JavaScript owns the table data
-- (a JSON export) and passes one record and one list of summaries per
-- request in the compact formats of "Modular.CP".
module Web (render, svg, identifyKey) where

import qualified Data.ByteString.Unsafe as BU
import Data.ByteString.Builder (Builder, toLazyByteString)
import qualified Data.ByteString.Lazy as BL
import Foreign.Ptr (Ptr, castPtr)
import Data.Word (Word8)
import GHC.Wasm.Prim
import Modular.CP (parseCompact, parseSummaries, parseOptions, parseCandidates, parseNames)
import Modular.Domain (enumerate)
import Modular.Identify (keyOf, showKey)
import Render.Page
import Web.Context (Context)
import Web.Group (contextFrom, groupOnly)
import Web.Params

-- `sync`: the export returns a string, not a Promise.
foreign export javascript "render sync" render :: JSString -> JSString -> JSString -> IO JSString
foreign export javascript "svg sync"    svg    :: JSString -> JSString -> JSString -> IO JSString
foreign export javascript "identifyKey sync" identifyKey :: JSString -> IO JSString

context :: JSString -> JSString -> JSString -> Context
context q r s = contextFrom (parseParams (parseQuery (fromJSString q))) rec (parseSummaries txt, parseOptions txt) (parseCandidates txt) (parseNames txt)
  where
    txt = fromJSString s
    rec = case fromJSString r of
      "" -> Left "choose a group"
      t  -> parseCompact t

-- | @genus|level|index|widths@ of the group of modes 1 and 3, so the page
-- can pick the table entries worth comparing; empty when there is none.
identifyKey :: JSString -> IO JSString
identifyKey q = pure . toJSString $ case groupOnly (parseParams (parseQuery (fromJSString q))) >>= enumerate of
  Right dom -> showKey (keyOf dom)
  Left _    -> ""

-- | The page is megabytes of path data; handing it over as a Haskell
-- 'String' meant a boxed character for each byte of it, and 'toJSString'
-- then walking that list.  The builder writes UTF-8 into one buffer and the
-- browser decodes it in a single call.
foreign import javascript unsafe "(new TextDecoder('utf-8')).decode(new Uint8Array(__exports.memory.buffer, $1, $2))"
  jsDecodeUtf8 :: Ptr Word8 -> Int -> IO JSString

fromBuilder :: Builder -> IO JSString
fromBuilder b = BU.unsafeUseAsCStringLen (BL.toStrict (toLazyByteString b)) $ \(p, n) -> jsDecodeUtf8 (castPtr p) n

render :: JSString -> JSString -> JSString -> IO JSString
render q r s = fromBuilder (renderPage (context q r s))

svg :: JSString -> JSString -> JSString -> IO JSString
svg q r s = fromBuilder (renderSvgOnly (context q r s))

