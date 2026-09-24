-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | The page: a pure function from a 'Context' to HTML.
--
-- Mode 1 reproduces the controls of Helena A. Verrill's Fundamental Domain
-- Drawer (Java applet, 2001): the group choices, Draw, edit, links, the
-- colour choices, and the triangle explorer with its MT, MS, TM, ...
-- buttons.
--
-- There is no JavaScript. Every control is a link carrying the next state
-- in its query string, or a GET form that produces one. The server (and,
-- later, the wasm build) only has to call 'renderPage'.
--
-- Three modes, chosen by the slider at the top of the left panel:
--
--   1. the classical families Γ₀, Γ₁, Γ⁰, Γ¹, Γ and their intersections;
--   2. the Cummins–Pauli tables, browsed genus → level → index → group;
--   3. generators typed in, certified congruence or not, and drawn.
--
-- The view — upper half-plane or disk — and everything under "Show" are
-- global to all three.
module Render.Page
  ( renderPage, renderSvgOnly, sceneFor
  ) where

import Data.Array ((!))
import Data.ByteString.Builder
import Data.List (intercalate, sortOn, stripPrefix)
import Data.Ratio
import Flint.Ball
import Flint.SL2
import Flint.Z
import Modular.CP (Record(..), Summary(..), Options(..), displayName, specialTex, parseName)
import Modular.Identify (identify)
import Modular.Disk
import Modular.Domain
import Modular.Generators (examples, parseGenerators)
import Modular.Geometry
import Modular.Group
import Modular.Word (wordOf)
import Render.Svg
import Web.Context
import Web.Params

-- Building --------------------------------------------------------------------

-- | The view; with no zoom or centre in the URL, one that fits the given
-- triangles, between 40 and 400 px per unit.
viewOf :: Params -> [SL2] -> View
viewOf p gs = View { vW = pW p, vH = pH p, vY0 = 60, vScale = sc, vCx = cx }
  where
    (cx, sc)
      | pAuto p && not (null gs) = let (c', s') = fitView (pW p) gs in (c', max 40 (min 400 s'))
      | otherwise = (pCx p, pScale p)

-- | The parameters with the view pinned, so that every link from an
-- auto-fitted page keeps that view rather than refitting.
pinned :: Params -> View -> Params
pinned p v = p { pScale = vScale v, pCx = vCx v, pAuto = False }

diskOf :: Params -> DiskView
diskOf p = DiskView (fromIntegral (pW p) / 2) (fromIntegral (pH p) / 2) (fromIntegral (min (pW p) (pH p)) / 2 - 8)

data Built = Built Domain Info

build :: Context -> Either String Built
build cx = do
  sg  <- cxGroup cx
  dom <- enumerate sg
  let dom' = applyMoves (pMoves (cxParams cx)) dom
  return (Built dom' (info dom'))

sideName :: Gen -> String
sideName T    = "T (right side)"
sideName Tinv = "T⁻¹ (left side)"
sideName S    = "S (bottom arc)"

-- | The scene for the current mode.
sceneFor :: Context -> Either String Scene
sceneFor cx = case pMode (cxParams cx) of
  TriMode    -> Right (sceneTri (cxParams cx))
  DomainMode -> sceneDomain (cxParams cx) <$> build cx

sceneDomain :: Params -> Built -> Scene
sceneDomain p0 b@(Built dom inf) = case pView p0 of
  Disk -> sceneDomainDisk p0 b
  UHP  -> uhpScene v tris links dots cuspLabels note
  where
    v    = viewOf p0 (map (dReps dom !) [0 .. size dom - 1])
    p    = pinned p0 v
    prec = precFor v
    n    = size dom
    reps = dReps dom
    sel  = pSel p
    tris = [ Tri (triangleShape prec v g) (colourCss (pFill p) i) (stroke i) (width i)
                 (Just (href p { pSel = Just i })) (title i g)
           | i <- [0 .. n - 1], let g = reps ! i, visible v g ]
    stroke i | sel == Just i = "#0044ff"
             | otherwise     = colourCss (pOutline p) i
    width i  | sel == Just i = 2.5
             | otherwise     = 0.9
    title i g = "#" ++ show i ++ "  " ++ showMat g ++ "  cusp " ++ showQI (cusp g)
    -- each unglued pair once; an S-side paired with itself is an elliptic point, not a curve
    links | pLinks p =
              [ (edgePoint prec v (reps ! i) g, edgePoint prec v (reps ! j) (genInverse g))
              | i <- [0 .. n - 1], g <- gens
              , let e = edge dom i g, not (eGlued e), let j = eNbr e
              , once i g j, visible v (reps ! i) || visible v (reps ! j) ]
          | otherwise = []
    once i g j = case g of
      T    -> j >= i
      Tinv -> j > i
      S    -> j > i
    dots  | pEdit p =
              [ Dot (edgePoint prec v (reps ! i) g) hrefOf titleOf
              | i <- [0 .. n - 1], g <- gens
              , let e = edge dom i g, not (eGlued e), let j = eNbr e, visible v (reps ! i)
              , let hrefOf | j == i    = Nothing
                           | otherwise = Just (href p { pMoves = pMoves p ++ [(i, g)] })
              , let titleOf
                      | j == i, g == S = "elliptic point of order 2: this side is paired with itself"
                      | j == i         = "cusp of width 1: paired with the opposite side of this triangle"
                      | otherwise      = "draw #" ++ show j ++ " (and what hangs off it) across the " ++ sideName g ++ " of #" ++ show i ]
          | otherwise = []
    cuspLabels
      | pCuspLabels p && length (iCusps inf) <= 80 =
          [ (projectX v x, showQI (cValue c)) | c <- iCusps inf, Just x <- [finite (cValue c)] ]
      | otherwise = []
    note = subgroupName (dGroup dom) ++ "    index " ++ show n ++ "    genus " ++ show (iGenus inf)
           ++ "    cusps " ++ show (length (iCusps inf))

-- | The domain in the disk: the (2,3,∞) tessellation underneath if asked,
-- the Γ-translates of the domain in two colours if asked, the domain on top.
sceneDomainDisk :: Params -> Built -> Scene
sceneDomainDisk p0 (Built dom inf) =
  Scene v tris 0.6 links dots [] note False (Just (dv, bgColour)) layers
  where
    v    = viewOf p0 []
    p    = p0 { pAuto = False }
    dv   = diskOf p
    prec = precForDisk dv
    n    = size dom
    reps = dReps dom
    sel  = pSel p
    bgColour = if pBg p then colourCss (pC1 p) 0 else "#ffffff"
    tris = [ Tri (triangleDisk prec dv g) (colourCss (pFill p) i) (stroke i) (width i)
                 (Just (href p { pSel = Just i })) (title i g)
           | i <- [0 .. n - 1], let g = reps ! i ]
    stroke i | sel == Just i = "#0044ff"
             | otherwise     = colourCss (pOutline p) i
    width i  | sel == Just i = 2.5
             | otherwise     = 0.9
    title i g = "#" ++ show i ++ "  " ++ showMat g ++ "  cusp " ++ showQI (cusp g)
    links | pLinks p =
              [ (edgePointDisk prec dv (reps ! i) g, edgePointDisk prec dv (reps ! j) (genInverse g))
              | i <- [0 .. n - 1], g <- gens, let e = edge dom i g, not (eGlued e), let j = eNbr e
              , case g of { T -> j >= i; Tinv -> j > i; S -> j > i } ]
          | otherwise = []
    dots  | pEdit p =
              [ Dot (edgePointDisk prec dv (reps ! i) g)
                    (if j == i then Nothing else Just (href p { pMoves = pMoves p ++ [(i, g)] }))
                    (if j == i then "paired with itself" else "draw #" ++ show j ++ " (and what hangs off it) across the " ++ sideName g ++ " of #" ++ show i)
              | i <- [0 .. n - 1], g <- gens, let e = edge dom i g, not (eGlued e), let j = eNbr e ]
          | otherwise = []
    -- two parts, marked in the SVG: the build serves each as a file of its own, shared by every page that has it
    layers = [("tess", tessellation), ("tile", tiling)]
    tessellation
      | pBg p = [ Layer (colourCss (pC2 p) 0) (colourCss (pC2 p) 0) 0.4 1 True (modularTiles prec dv 1.5 6000) ]
      | otherwise = []
    -- the translates, all triangles of each, then every translate's boundary sides
    tiling
      | pTile p =
          let cap = max 1 (5000 `div` n)
              ts  = translates prec dv dom 2 cap
              colourAt d = colourCss (if even d then pFill p else pFill2 p) d
              body = [ Layer (colourAt d) (colourAt d) 0.4 0.55 True [ triangleDisk prec dv (γ `mul` (reps ! k)) | k <- [0 .. n - 1] ]
                     | (d, γ) <- ts, d > 0 ]
              rim  = Layer "none" (colourCss (pOutline p) 0) 0.8 1 False
                       [ Shape a [seg] Nothing
                       | (_, γ) <- ts, k <- [0 .. n - 1], g <- gens, not (eGlued (edge dom k g))
                       , let (a, seg) = sideDisk prec dv (γ `mul` (reps ! k)) g ]
          in body ++ [rim]
      | otherwise = []
    note = subgroupName (dGroup dom) ++ "    index " ++ show n ++ "    genus " ++ show (iGenus inf)
           ++ "    cusps " ++ show (length (iCusps inf))

sceneTri :: Params -> Scene
sceneTri p = case pView p of
  UHP  -> uhpScene v tris [] [] [] note
  Disk -> Scene v trisD 0.6 [] [] [] note False (Just (dv, if pBg p then colourCss (pC1 p) 0 else "#ffffff"))
                [ ("tess", [ Layer (colourCss (pC2 p) 0) (colourCss (pC2 p) 0) 0.4 1 True (modularTiles precD dv 1.5 6000) | pBg p ]) ]
  where
    v    = viewOf p (pMats p)
    prec = precFor v
    dv   = diskOf p
    precD = precForDisk dv
    mats = pMats p
    k    = length mats
    style i = ( if i == k - 1 then colourCss (pFill p) i else "#dddddd"
              , colourCss (pOutline p) i
              , if i == k - 1 then 2 else 0.9 )
    tris  = [ Tri (triangleShape prec v g) f o w Nothing (showMat g) | (i, g) <- zip [0 ..] mats, visible v g, let (f, o, w) = style i ]
    trisD = [ Tri (triangleDisk precD dv g) f o w Nothing (showMat g) | (i, g) <- zip [0 ..] mats, let (f, o, w) = style i ]
    note = "triangle explorer    M = " ++ showMat (last mats)

renderSvgOnly :: Context -> Builder
renderSvgOnly cx = case sceneFor cx of
  Right sc -> renderSvg sc
  Left err -> stringUtf8 "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"400\" height=\"60\"><text x=\"10\" y=\"30\">" <> escB err <> stringUtf8 "</text></svg>"

-- HTML ------------------------------------------------------------------------

esc :: String -> String
esc = concatMap e
  where
    e '<' = "&lt;"
    e '>' = "&gt;"
    e '&' = "&amp;"
    e '"' = "&quot;"
    e ch  = [ch]

el :: String -> [(String, String)] -> String -> String
el tag attrs body = "<" ++ tag ++ concat [ " " ++ k ++ "=\"" ++ esc v ++ "\"" | (k, v) <- attrs ] ++ ">" ++ body ++ "</" ++ tag ++ ">"

btn :: String -> String -> String
btn u label = el "a" [("class", "btn"), ("href", u)] (esc label)

toggle :: Bool -> String -> String -> String
toggle on u label = el "a" [("class", if on then "btn on" else "btn"), ("href", u)] (esc label)

hidden :: String -> String -> String
hidden k v = "<input type=\"hidden\" name=\"" ++ k ++ "\" value=\"" ++ esc v ++ "\">"

select :: String -> [(String, String)] -> String -> String
select name opts cur = el "select" [("name", name)] $ concat
  [ "<option value=\"" ++ esc v ++ "\"" ++ (if v == cur then " selected" else "") ++ ">" ++ esc label ++ "</option>" | (v, label) <- opts ]

-- | A select that redraws the moment it changes: the forms of the families have no button.
live :: String -> String
live s = case stripPrefix "<select" s of
  Just rest -> "<select onchange=\"this.form.requestSubmit()\"" ++ rest
  Nothing   -> s

-- | The attributes of a typed field that redraws when its value is committed: on change (leaving the field, a
-- spinner step), and on Enter, which a form with two fields and no button would otherwise ignore.
onCommit :: String
onCommit = " onchange=\"this.form.requestSubmit()\" onkeydown=\"if(event.key==='Enter'){event.preventDefault();this.form.requestSubmit()}\""

css :: String
css = unlines
  [ "body{font-family:-apple-system,Helvetica,Arial,sans-serif;margin:0;background:#f4f4f7;color:#222}"
  , "header{padding:8px 16px;background:#243b6b;color:#fff;display:flex;justify-content:space-between;align-items:baseline}"
  , "header h1{margin:0;font-size:18px;font-weight:600} header span{font-size:12px;opacity:.8}"
  , ".layout{display:grid;grid-template-columns:300px minmax(0,1fr) 340px;gap:12px;padding:12px;align-items:start}"
  , ".panel{background:#fff;border:1px solid #d7d7de;border-radius:8px;padding:10px 12px;font-size:13px}"
  , ".panel h2{font-size:14px;margin:6px 0 6px;color:#243b6b}"
  , ".btn{display:inline-block;padding:4px 9px;margin:2px;border:1px solid #9aa;border-radius:5px;background:#eef;color:#123;text-decoration:none;font-size:13px}"
  , ".btn.on{background:#243b6b;color:#fff;border-color:#243b6b} .btn:hover{background:#dde} .btn .katex{font-size:1em}"
  , ".slider{display:flex;border:1px solid #243b6b;border-radius:20px;overflow:hidden;margin:2px 0 10px}"
  , ".slider a{flex:1;text-align:center;padding:7px 4px;text-decoration:none;color:#243b6b;font-size:13px;background:#fff;border-right:1px solid #cfd5e5}"
  , ".slider a:last-child{border-right:none} .slider a.on{background:#243b6b;color:#fff}"
  , ".chain{line-height:2.1}.chain a{margin-right:2px}"
  , ".pills{display:flex;gap:6px;flex-wrap:wrap;margin:6px 0 8px}"
  , ".pill{display:flex;flex-direction:column;align-items:flex-start;padding:5px 9px;border:1px solid #243b6b;border-radius:16px;background:#fff;font-size:13px;color:#123;min-width:64px;text-decoration:none}"
  , ".pill small{font-size:10px;color:#667;text-transform:uppercase;letter-spacing:.05em}"
  , ".pill select{border:none;background:transparent;font-size:13px;padding:0;margin:0;max-width:220px}"
  , ".pill input[type=number]{border:none;background:transparent;font-size:13px;padding:0;margin:0;width:4.5em}"
  , ".pill.cap{justify-content:center;align-items:center;min-width:0;padding:5px 7px;border-color:transparent;background:transparent;font-size:15px}"
  , ".pills.family{display:grid;grid-template-columns:1fr auto 1fr;gap:6px;align-items:stretch} .pills.family .pill{min-width:0}"
  , ".pills.family .pill select,.pills.family .pill input[type=number]{width:100%;box-sizing:border-box}"
  , "button{padding:5px 12px;font-size:13px;border-radius:5px;border:1px solid #243b6b;background:#243b6b;color:#fff;margin-top:6px}"
  , "input,select,textarea{font-size:13px;padding:3px 5px;margin:2px 0} input[type=number]{width:5.5em} input.rat{width:8em} input.ent{width:4em}"
  , "textarea{width:100%;box-sizing:border-box;font-family:ui-monospace,Menlo,monospace}"
  , "label{display:block;margin:4px 0}"
  , "table.kv td,table.kv th{padding:2px 6px;font-size:13px;vertical-align:top;text-align:left} table.kv td:first-child{color:#555}"
  , ".cusps{line-height:1.9}.cusps a{margin-right:8px;white-space:nowrap}"
  , ".class a{display:block;padding:3px 6px;border-radius:4px;text-decoration:none;color:#123}.class a.on{background:#243b6b;color:#fff}.class a:hover{background:#dde}"
  , "svg{max-width:100%;height:auto;border:1px solid #d7d7de;border-radius:6px;background:#fff}"
  , ".mat{font-family:ui-monospace,Menlo,monospace;font-size:13px}"
  , ".warn{color:#b00} .ok{color:#080} .muted{color:#666}"
  , "table.cp td:first-child{background:#d5e6f7;color:#123;text-align:center;padding:4px 8px;border-radius:3px} table.cp th{background:#c9e8b8;padding:4px 8px}"
  , ".layout>*{min-width:0} .go{display:inline-flex;gap:4px;align-items:center;margin:0} .go select{font-size:13px;max-width:200px} .go button{margin-top:0;padding:3px 9px} .nb{white-space:nowrap}"
  , "@media (max-width:1150px){.layout{grid-template-columns:1fr}}"
  ]

-- | KaTeX from a CDN, rendering @\\( … \\)@ once loaded. The single-page
-- build calls @renderMathInElement@ again after every render.
katexHead :: String
katexHead = unlines
  [ "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css\" crossorigin=\"anonymous\">"
  , "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js\" crossorigin=\"anonymous\"></script>"
  , "<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/contrib/auto-render.min.js\" crossorigin=\"anonymous\""
    ++ " onload=\"window.fundomMath=function(el){renderMathInElement(el,{delimiters:[{left:'\\\\(',right:'\\\\)',display:false},{left:'\\\\[',right:'\\\\]',display:true}],throwOnError:false})};fundomMath(document.body)\"></script>"
  ]

-- | A matrix, for KaTeX.
texMat :: SL2 -> String
texMat g = "\\begin{pmatrix}" ++ show a' ++ "&" ++ show b' ++ "\\\\" ++ show c' ++ "&" ++ show d' ++ "\\end{pmatrix}"
  where (a', b', c', d') = entries g

-- | A word in S and R, for KaTeX.
texWord :: SL2 -> String
texWord g = case wordOf g of
  [] -> "1"
  w  -> intercalate "\\," (map letter w)
  where
    letter l = case show l of
      "LS"  -> "S"
      "LR"  -> "R"
      _     -> "R^{-1}"

inlineTex :: String -> String
inlineTex t = "\\(" ++ t ++ "\\)"

-- | Formulas in a row that may wrap: each is typeset on its own and keeps
-- its comma, so a line breaks only between them.
formulas :: [String] -> String
formulas = unwords . units "" ""

-- | A presentation ⟨g₁, …, gₙ⟩ that wraps between generators.
presentation :: [String] -> String
presentation = unwords . units (esc (inlineTex "\\bigl\\langle") ++ "&nbsp;") ("&nbsp;" ++ esc (inlineTex "\\bigr\\rangle"))

-- | Each formula with its comma (the first with an opening, the last with
-- a closing) as an unbreakable unit.
units :: String -> String -> [String] -> [String]
units open close ts =
  [ el "span" [("class", "nb")] (o ++ esc (inlineTex t) ++ c)
  | (i, t) <- zip [1 :: Int ..] ts
  , let o = if i == 1 then open else ""
        c = if i == length ts then close else "," ]

-- | The whole page. The panels are a few kilobytes and are built as
-- 'String's, as they always were; the picture is a 'Builder' (see
-- "Render.Svg"), and is not copied into one.
renderPage :: Context -> Builder
renderPage cx = nl
  [ utf8 "<!doctype html>"
  , utf8 "<html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , utf8 ("<title>Fundamental domains</title><style>" ++ css ++ "</style>")
  , utf8 katexHead
  , utf8 "</head><body>"
  , utf8 "<header><h1>Fundamental domains of congruence subgroups of SL₂(ℤ)</h1></header>"
  , utf8 "<div class=\"layout\">"
  , utf8 (el "div" [("class", "panel")] (leftPanel cx p))
  , utf8 "<div id=\"plot\">" <> either (utf8 . errorBox) renderSvg scene <> utf8 "</div>"
  , utf8 (el "div" [("class", "panel")] (rightPanel cx p built))
  , utf8 "</div></body></html>"
  ]
  where
    utf8 = stringUtf8
    nl bs = mconcat [ b <> char8 '\n' | b <- bs ]     -- as `unlines` made them
    p0    = cxParams cx
    built = build cx
    scene = sceneFor cx
    -- the panels link with the view the picture was actually drawn at
    p = case scene of
      Right sc -> pinned p0 (scView sc)
      Left _   -> p0
    errorBox err = el "div" [("class", "panel warn")] (esc err)

-- The left panel ------------------------------------------------------------------

modeSlider :: Params -> String
modeSlider p = el "div" [("class", "slider")] $ concat
  [ el "a" [("class", if pApp p == k then "on" else ""), ("href", href (fresh p) { pApp = k })] (esc label)
  | (k, label) <- [(1, "families"), (2, "tables"), (3, "by generators")] ]

leftPanel :: Context -> Params -> String
leftPanel cx p = concat
  [ modeSlider p
  , case pApp p of
      2 -> browseControls cx p
      3 -> generatorControls p
      _ -> familyControls p
  , viewControls cx p
  , showControls p
  , if pApp p == 1 then modeControls p else ""
  , el "p" [("class", "muted")] "Click a triangle for its matrix. In edit mode, click a yellow marker to redraw the triangle paired with that side there."
  , el "p" [("class", "muted")] $ "<a href=\"" ++ esc (svgHref p) ++ "\">SVG only</a>"
  , el "p" [("class", "muted")] "Algorithm and original applet: Helena A. Verrill, <i>Algorithm for Drawing Fundamental Domains</i>, January 2001. Tables: C. Cummins and S. Pauli. Congruence test: T. Hsu, Proc. AMS 124 (1996)."
  ]

-- | Mode 1: the classical families — Helena A. Verrill's applet.
familyControls :: Params -> String
familyControls p = concat
  [ el "p" [("class", "muted")] ("This mode is a port of " ++ el "b" [] "Helena A. Verrill's" ++ " Fundamental Domain Drawer (Java applet, 2001): the choice of groups, the walk that lays out the triangles, the side-pairing table, the edit and link modes and the triangle explorer are hers, following her <i>Algorithm for Drawing Fundamental Domains</i> (January 2001).")
  , el "h2" [] "Group"
  , el "form" [("method", "get"), ("action", "")] $ concat
      -- two rows on one grid: N sits under the first type, M under the second
      [ el "div" [("class", "pills family")] $ concat
          [ el "label" [("class", "pill")] (el "small" [] "type" ++ dropdown "g1" [ (typeCode t, typeLabel t "N") | t <- allTypes ] (Just (typeCode (pG1 p))) "")
          , el "span" [("class", "pill cap")] "∩"
          , el "label" [("class", "pill")] (el "small" [] "type" ++ dropdown "g2" [ (typeCode t, typeLabel t "M") | t <- allTypes ] (Just (typeCode (pG2 p))) "")
          , el "label" [("class", "pill")] (el "small" [] "N" ++ "<input type=\"number\" name=\"n\" min=\"1\" value=\"" ++ show (pN p) ++ "\"" ++ onCommit ++ ">")
          , el "span" [("class", "pill cap")] ""
          , el "label" [("class", "pill")] (el "small" [] "M" ++ "<input type=\"number\" name=\"m\" min=\"1\" value=\"" ++ show (pM p) ++ "\"" ++ onCommit ++ ">")
          ]
      , concat [ hidden k v | (k, v) <- carried p ]
      ]
  , el "p" [] $ "N " ++ btn (href (fresh p) { pN = max 1 (pN p - 1) }) "−" ++ btn (href (fresh p) { pN = pN p + 1 }) "+"
             ++ "  M " ++ btn (href (fresh p) { pM = max 1 (pM p - 1) }) "−" ++ btn (href (fresh p) { pM = pM p + 1 }) "+"
  ]

-- | Mode 2: a row of four pills — genus, level, index, group. Each carries a
-- dropdown once it is in play. Nothing is chosen at first and only one
-- category is open (genus, unless the URL says @by=lev@ or @by=idx@); the
-- others are greyed, and clicking a greyed one opens it instead, so a
-- search can start from any category. A chosen category keeps its dropdown,
-- now offering only values consistent with the other choices, plus "any"
-- to drop it. The group pill lists the groups matching every choice.
browseControls :: Context -> Params -> String
browseControls cx p = concat
  [ el "h2" [] "Cummins–Pauli tables"
  , el "form" [("method", "get"), ("action", "")] $ concat
      [ hidden "app" "2"
      , el "div" [("class", "pills")] (concat
          [ cell "gen" "genus" (pGenus p) (oGenera opts)
          , cell "lev" "level" (pLevel p) (oLevels opts)
          , cell "idx" "index" (pIndex p) (oIndices opts)
          , el "label" [("class", "pill")] (el "small" [] "group"
              ++ dropdown "db" (outside ++ [ (smName sm, describe sm) | sm <- cxClass cx ]) (pDb p) (if null (cxClass cx) then "no match" else "group…")) ])
      , concat [ hidden k v | (k, v) <- carried p ]
      , "<noscript><button type=\"submit\">Show</button></noscript>"
      ]
  , el "p" [("class", "muted")] (if anyChosen
      then show n ++ (if n == 1 then " group matches." else " groups match.")
      else show n ++ " groups: every congruence subgroup of PSL₂(ℤ) of genus ≤ 24, up to conjugacy. Each choice narrows the others.")
  , el "h2" [] "Or by name"
  , el "form" [("method", "get"), ("action", "")] $ concat
      [ hidden "app" "2"
      , el "label" [] ("<input class=\"ent\" name=\"db\" placeholder=\"11A1\" value=\"" ++ esc (maybe "" id (pDb p)) ++ "\"> <button type=\"submit\">Load</button>")
      , concat [ hidden k v | (k, v) <- carried p, k `notElem` ["gen", "lev", "idx"] ]
      ]
  ]
  where
    opts = cxOptions cx
    n = length (cxClass cx)
    anyChosen = any (/= Nothing) [pGenus p, pLevel p, pIndex p]
    -- every category is a dropdown with "any"; each choice narrows the others' options and the groups offered
    cell key label chosen values = el "label" [("class", "pill")] (el "small" [] label ++ dropdownAny key [ (show x, show x) | x <- values ] (maybe "" show chosen))
    describe sm = displayName (smName sm) (smSpecial sm)
                  ++ " · index " ++ show (smIndex sm) ++ ", genus " ++ show (smGenus sm) ++ ", level " ++ show (smLevel sm)
    -- the group on screen stays in the list when the filters leave it out, so the list names what is drawn and a
    -- filter changed back does not lose it
    outside = [ (rName r, displayName (rName r) (rSpecial r) ++ " · outside these filters")
              | Just r <- [cxRecord cx], rName r `notElem` map smName (cxClass cx) ]

-- | A select that submits its form on change, with a placeholder when
-- nothing is chosen yet.
dropdown :: String -> [(String, String)] -> Maybe String -> String -> String
dropdown name opts cur0 placeholder =
  el "select" [("name", name), ("onchange", "this.form.requestSubmit()")] $
    (case cur of
       Just _  -> ""
       Nothing -> "<option value=\"\" selected disabled>" ++ esc placeholder ++ "</option>")
    ++ concat [ "<option value=\"" ++ esc v ++ "\"" ++ (if Just v == cur then " selected" else "") ++ ">" ++ esc label ++ "</option>" | (v, label) <- opts ]
  where
    -- a value that is not among the options (a group the filters have since left out) is not shown as chosen
    cur = if maybe False (`elem` map fst opts) cur0 then cur0 else Nothing

-- | The same, for a chosen category: "any" drops the filter.
dropdownAny :: String -> [(String, String)] -> String -> String
dropdownAny name opts cur =
  el "select" [("name", name), ("onchange", "this.form.requestSubmit()")] $
    "<option value=\"\">any</option>"
    ++ concat [ "<option value=\"" ++ esc v ++ "\"" ++ (if v == cur then " selected" else "") ++ ">" ++ esc label ++ "</option>" | (v, label) <- opts ]

-- | Mode 3: generators.
generatorControls :: Params -> String
generatorControls p = concat
  [ el "h2" [] "Generators"
  , el "p" [("class", "muted")] "Integer matrices of determinant 1, one per line as a b c d. The subgroup they generate is enumerated; if its index is finite, Hsu's criterion decides whether it is a congruence subgroup, and it is drawn either way."
  , el "form" [("method", "get"), ("action", "")] $ concat
      [ hidden "app" "3"
      , "<textarea name=\"gens\" rows=\"7\" placeholder=\"1 1 0 1\n1 0 2 1\">" ++ esc (pGens p) ++ "</textarea>"
      , concat [ hidden k v | (k, v) <- carried p ]
      , "<button type=\"submit\">Certify and draw</button>"
      ]
  , el "h2" [] "Examples"
  , el "div" [("class", "chain")] $ concat
      [ el "a" [("class", "btn"), ("href", href (fresh p) { pApp = 3, pGens = g })] (presentation mats) | (mats, g) <- examples ]
  ]

-- | The view, in every mode.
viewControls :: Context -> Params -> String
viewControls cx p = concat
  [ el "h2" [] "View"
  , el "p" [] $ toggle (pView p == UHP) (href p { pView = UHP }) "upper half-plane"
              ++ toggle (pView p == Disk) (href p { pView = Disk }) "disk"
  , case pView p of
      UHP -> el "p" [] (concat
               [ btn (href (pan (-4))) "⇐", btn (href (pan (-1))) "←", btn (href p { pCx = 0 }) "0"
               , btn (href (pan 1)) "→", btn (href (pan 4)) "⇒" ])
             ++ el "p" [] (concat
               [ btn (href p { pScale = pScale p * 2 }) "zoom in", btn (href p { pScale = pScale p / 2 }) "zoom out"
               , btn (href p { pScale = 50, pCx = 0 }) "reset", fitBtn ])
             ++ el "form" [("method", "get"), ("action", "")] (concat
                  [ el "label" [] ("scale <input class=\"rat\" name=\"scale\" value=\"" ++ esc (showRat (pScale p)) ++ "\"" ++ onCommit ++ "> px per unit")
                  , el "label" [] ("centre <input class=\"rat\" name=\"cx\" value=\"" ++ esc (showRat (pCx p)) ++ "\"" ++ onCommit ++ ">")
                  , el "label" [] ("domain " ++ live (select "fill" [ (c', c') | c' <- colourNames ] (pFill p)) ++ " / " ++ live (select "outline" [ (c', c') | c' <- colourNames ] (pOutline p)))
                  , carry ["scale", "cx", "fill", "outline"] ])
      Disk -> el "p" [] (toggle (pBg p) (href p { pBg = not (pBg p) }) "modular tessellation"
                         ++ toggle (pTile p) (href p { pTile = not (pTile p) }) "tile by Γ")
              ++ el "form" [("method", "get"), ("action", "")] (concat
                   [ el "label" [] ("tessellation " ++ live (select "c1" [ (c', c') | c' <- colourNames ] (pC1 p)) ++ " / " ++ live (select "c2" [ (c', c') | c' <- colourNames ] (pC2 p)))
                   , el "label" [] ("translates " ++ live (select "fill" [ (c', c') | c' <- colourNames ] (pFill p)) ++ " / " ++ live (select "fill2" [ (c', c') | c' <- colourNames ] (pFill2 p)))
                   , el "label" [] ("outline " ++ live (select "outline" [ (c', c') | c' <- colourNames ] (pOutline p)))
                   , carry ["c1", "c2", "fill", "fill2", "outline"] ])
  ]
  where
    -- a GET form resets what it does not carry, so every field it does not own is hidden in it
    carry own = concat [ hidden k v | (k, v) <- parseQuery (toQuery p), k `notElem` own ]
    pan k = p { pCx = pCx p + fromIntegral (k :: Int) * (fromIntegral (pW p) / 8) / pScale p }
    fitBtn = case build cx of
      Right (Built dom _) | pMode p == DomainMode ->
        let (cx', sc) = fitView (pW p) (map (dReps dom !) [0 .. size dom - 1])
        in btn (href p { pCx = cx', pScale = sc }) "fit"
      _ -> ""

showControls :: Params -> String
showControls p = concat
  [ el "h2" [] "Show"
  , el "p" [] $ concat
      [ toggle (pLinks p) (href p { pLinks = not (pLinks p) }) "side pairings"
      , toggle (pEdit p) (href p { pEdit = not (pEdit p) }) "edit"
      , toggle (pCuspLabels p) (href p { pCuspLabels = not (pCuspLabels p) }) "cusp labels" ]
  , if null (pMoves p) then "" else
      el "p" [] $ show (length (pMoves p)) ++ " rearrangement" ++ (if length (pMoves p) == 1 then "" else "s") ++ " "
                  ++ btn (href p { pMoves = init (pMoves p) }) "undo" ++ btn (href p { pMoves = [] }) "clear"
  ]

modeControls :: Params -> String
modeControls p = concat
  [ el "h2" [] "Mode"
  , el "p" [] $ toggle (pMode p == DomainMode) (href p { pMode = DomainMode }) "domain"
              ++ toggle (pMode p == TriMode) (href p { pMode = TriMode }) "triangle explorer"
  ]

-- | State a GET form must carry, since a submit otherwise resets it: the
-- view settings, but not the zoom, which a new group fits afresh.
carried :: Params -> [(String, String)]
carried p = [ (k, v) | (k, v) <- parseQuery (toQuery p)
            , k `notElem` ["sel", "mv", "n", "m", "g1", "g2", "db", "gens", "app", "gen", "lev", "idx", "mode", "mats", "copy", "scale", "cx"] ]

-- | A change of group starts over: no selection, no rearrangements, no
-- explorer, no table lookup, and the picture fitted to the view again.
fresh :: Params -> Params
fresh p = p { pSel = Nothing, pMoves = [], pMode = DomainMode, pDb = Nothing, pGenus = Nothing, pLevel = Nothing, pIndex = Nothing
            , pScale = pScale defaultParams, pCx = 0, pAuto = True }

svgHref :: Params -> String
svgHref p = "svg?" ++ toQuery p

-- The right panel ---------------------------------------------------------------------

rightPanel :: Context -> Params -> Either String Built -> String
rightPanel cx p built = case pApp p of
  2 -> either (const (el "p" [("class", "muted")] "Choose a group in the pills on the left, or type its name.")) (infoPanel cx p) built
  3 -> certificate cx p ++ either (const "") (infoPanel cx p) built
  _ -> case pMode p of
         TriMode    -> explorer p
         DomainMode -> either (const "") (infoPanel cx p) built

-- | Mode 3: what the enumeration and Hsu's criterion found.
certificate :: Context -> Params -> String
certificate cx p = case cxGroup cx of
  Left err -> el "h2" [] "Certificate" ++ el "p" [("class", "warn")] (esc err)
  Right sg -> concat
    [ el "h2" [] "Certificate"
    , el "p" [] (either esc (presentation . map texMat) (parseGenerators (pGens p)))
    , case sgVerdict sg of
        Just (lv, Nothing)  -> el "p" [("class", "ok")] ("Congruence: the group contains Γ(" ++ show lv ++ "). Hsu's relations for level " ++ show lv ++ " hold in the coset action.")
        Just (lv, Just why) -> el "p" [("class", "warn")] ("Not congruence: Hsu's relation " ++ esc why ++ " fails in the coset action, so the group does not contain Γ(" ++ show lv ++ "), and by Wohlfahrt's theorem contains no Γ(N) at all.")
        Nothing -> ""
    , el "p" [("class", "muted")] "The action of PSL₂(ℤ) on the cosets of the group is computed from the generators, and Hsu's criterion is applied to it. The level shown is the lcm of the cusp widths, which is the level whenever the group is congruence."
    ]

-- | The right panel: the group as an entry of the tables when it is one
-- (the tables' own format), the computed invariants otherwise, and the
-- selected triangle.
infoPanel :: Context -> Params -> Built -> String
infoPanel cx p (Built dom inf) = concat
  [ case (pApp p, cxRecord cx, found) of
      (2, Just r, _) -> cpTable cx p r False
                        ++ el "p" [("class", "muted")] "Entries are up to conjugacy in PGL₂(ℤ); the matrix generators define one representative."
      (_, _, Just r) -> el "h2" [] (esc (subgroupName sg))
                        ++ cpTable cx p r True
                        ++ el "p" [("class", "muted")] "An entry of the tables, up to conjugacy in PGL₂(ℤ); its name opens it there."
      _              -> el "h2" [] (esc (subgroupName sg)) ++ computed ++ el "p" [("class", "muted")] (esc whyNot)
  , mismatch
  , el "h2" [] "Cusps in the picture"
  , el "div" [("class", "cusps")] $ unwords
      [ el "a" [("href", href (goto (cValue c) (pScale p))), ("title", "centre on this cusp")]
              (esc (showQI (cValue c)) ++ el "span" [("class", "muted")] ("·" ++ show (cWidth c)))
      | c <- iCusps inf ]
  , case pSel p of
      Just i | i >= 0 && i < n -> selected i
      _ -> el "p" [("class", "muted")] "Click a triangle to see its matrix and its side pairings."
  ]
  where
    sg = dGroup dom
    n  = size dom
    row k v = el "tr" [] (el "td" [] (esc k) ++ el "td" [] v)
    found | pApp p == 2 = Nothing
          | otherwise   = identify dom (cxCandidates cx)
    computed = el "table" [("class", "kv")] $ concat
      [ row "level" (esc (show (sgLevel sg) ++ (case sgVerdict sg of { Just _ -> " (generalised)"; Nothing -> "" })))
      , row "index" (esc (show n ++ if containsMinusOne sg then "" else " (projective; " ++ show (2 * n) ++ " in SL₂(ℤ), as −I ∉ Γ)"))
      , row "genus" (show (iGenus inf))
      , row "cusps" (esc (inlineTex (partitionTex (map cWidth (iCusps inf)))))
      , row "c₂" (show (iE2 inf))
      , row "c₃" (show (iE3 inf))
      ]
    whyNot
      | Just (_, Just _) <- sgVerdict sg = "Not a congruence subgroup, so not in the tables."
      | iGenus inf > 24 = "Genus " ++ show (iGenus inf) ++ " is beyond the tables, which stop at 24."
      | null (cxCandidates cx) = "No entry of the tables has this genus, level, index and cusp widths."
      | otherwise = "Not conjugate to any entry of the tables with these invariants."
    -- the source's own figures are shown only if they disagree
    mismatch = case sgExpect sg of
      Nothing -> ""
      Just ex ->
        let ours   = sortDesc (map cWidth (iCusps inf))
            theirs = sortDesc (exCusps ex)
            diffs  = [ k ++ ": " ++ a ++ " here, " ++ b ++ " in " ++ exSource ex
                     | (k, a, b) <- [ ("index", show n, show (exIndex ex)), ("genus", show (iGenus inf), show (exGenus ex))
                                    , ("cusp widths", unwords (map show ours), unwords (map show theirs))
                                    , ("c₂", show (iE2 inf), show (exE2 ex)), ("c₃", show (iE3 inf), show (exE3 ex)) ]
                     , a /= b ]
        in concat [ el "p" [("class", "warn")] (esc d) | d <- diffs ]
    sortDesc = reverse . sortOn id
    goto q sc = case q of
      Fin x -> p { pCx = x, pScale = sc }
      Inf   -> p { pCx = 0, pScale = sc }
    selected i = concat
      [ el "h2" [] ("Triangle #" ++ show i)
      , el "p" [] (esc (inlineTex (texMat g ++ " = " ++ texWord g)))
      , el "table" [("class", "kv")] $ concat $
          row "cusp" (esc (showQI (cusp g))) :
          [ el "tr" [] (el "td" [] (esc (sideName gen)) ++ el "td" [] (pair gen)) | gen <- gens ]
      , el "p" [] $ btn (href (goto (cusp g) (pScale p))) "centre" ++ btn (href zoomTo) "zoom to"
                  ++ (if pApp p == 1 then btn (href p { pMode = TriMode, pMats = [g] }) "explore" else "")
                  ++ btn (href p { pSel = Nothing }) "deselect"
      ]
      where
        g = dReps dom ! i
        pair gen = let e = edge dom i gen
                       j = eNbr e
                   in el "a" [("href", href p { pSel = Just j })] ("#" ++ show j)
                      ++ (if eGlued e then " glued" else " paired, drawn elsewhere")
        zoomTo = let (lo, hi) = xExtent g
                     sc = (fromIntegral (pW p) * 2 / 5) / max (1 % 1000000000) (hi - lo)
                 in p { pCx = (lo + hi) / 2, pScale = sc }

-- The tables' format --------------------------------------------------------------

-- | A link to an entry of the tables, drawn afresh and fitted to the view.
toGroup :: Params -> String -> Params
toGroup p nm = (fresh p) { pApp = 2, pDb = Just nm }

-- | The name shown, as TeX: the classical one when there is one, else
-- @13\mathrm{A}^{24}@ — level, label, genus, as in the tables.
displayTex :: String -> Maybe String -> String
displayTex nm = maybe (nameTex nm) specialTex

nameTex :: String -> String
nameTex nm = case parseName nm of
  Just (lv, lab, gen) -> show lv ++ "\\mathrm{" ++ lab ++ "}^{" ++ show gen ++ "}"
  Nothing -> "\\mathrm{" ++ nm ++ "}"

-- | Cusp widths and Galois orbits in the tables' partition notation: @13^{42}@.
partitionTex :: [Int] -> String
partitionTex [] = "-"
partitionTex xs = intercalate "\\," [ show v ++ "^{" ++ show (length grp) ++ "}" | grp@(v : _) <- groupRuns (sortOn id xs) ]
  where
    groupRuns [] = []
    groupRuns (y : ys) = let (same, rest) = span (== y) ys in (y : same) : groupRuns rest

-- | One entry of the tables, in their columns, as a key–value table; the
-- name links to the entry in the browsing mode when asked.
cpTable :: Context -> Params -> Record -> Bool -> String
cpTable cx p r linked = el "table" [("class", "kv cp")] $ concat
  [ row "Name" (if linked then el "a" [("href", href (toGroup p (rName r))), ("title", "open in the tables")] name else name)
  , row "Index" (show (rIndex r))
  , row "con" (show (rCon r))
  , row "len" (show (rLen r))
  , row "c₂" (show (length (filter (== 1) (rC2 r))))
  , row "c₃" (show (length (filter (== 1) (rC3 r))))
  , row "Cusps" (esc (inlineTex (partitionTex (rCusps r))))
  , row "Gal" (esc (inlineTex (partitionTex (rGal r))))
  , row "Supergroups" (chooser (rSupers r))
  , row "Subgroups" (chooser (rSubs r))
  , row "Matrix generators" (if null (rMatGens r) then "—" else
      formulas [ "\\begin{pmatrix}" ++ show a ++ "&" ++ show b ++ "\\\\" ++ show c ++ "&" ++ show d ++ "\\end{pmatrix}" | (a, b, c, d) <- rMatGens r ]
      ++ " " ++ esc (inlineTex ("\\pmod{" ++ show (rLevel r) ++ "}")))
  ]
  where
    row k v = el "tr" [] (el "td" [] (esc k) ++ el "td" [] v)
    name = esc (inlineTex (displayTex (rName r) (rSpecial r)))
    -- a dropdown of the entries named and a button to go to the chosen one
    chooser [] = "—"
    chooser ns = el "form" [("method", "get"), ("action", ""), ("class", "go")] $ concat
      [ concat [ hidden k v | (k, v) <- parseQuery (toQuery (toGroup p "")), k /= "db" ]
      , el "select" [("name", "db")] (concat [ el "option" [("value", nm)] (esc (displayName nm (lookup nm (cxNames cx)))) | nm <- ns ])
      , "<button type=\"submit\">Go</button>"
      ]

-- | The right panel in explorer mode: one matrix, multiplied by generators.
explorer :: Params -> String
explorer p = concat
  [ el "h2" [] "Triangle explorer"
  , el "p" [("class", "muted")] "The triangle M·F for a matrix M, and its neighbours under the generators. “move” replaces it, “copy” keeps the previous ones."
  , el "form" [("method", "get"), ("action", "")] $ concat
      [ el "p" [] (esc (inlineTex ("M = " ++ texMat m ++ " = " ++ texWord m)))
      , el "p" [("class", "mat")] $ "[ " ++ box "a" pa ++ " " ++ box "b" pb ++ " ; " ++ box "c" pc ++ " " ++ box "d" pd ++ " ]"
      , concat [ hidden k v | (k, v) <- carriedTri ]
      , "<button type=\"submit\">Draw triangle</button>"
      ]
  , el "p" [] $ toggle (not (pCopy p)) (href p { pCopy = False }) "move" ++ toggle (pCopy p) (href p { pCopy = True }) "copy"
  , el "table" [("class", "kv")] $ concat
      [ el "tr" [] (el "td" [] "right" ++ el "td" [] (concat [ btn (href (next (m `mul` gm))) l | (l, gm) <- rights ]))
      , el "tr" [] (el "td" [] "left"  ++ el "td" [] (concat [ btn (href (next (gm `mul` m))) l | (l, gm) <- lefts ]))
      ]
  , el "p" [] $ btn (href p { pMats = [identity] }) "M = Id" ++ btn (href (goto (cusp m))) "centre" ++ btn (href p { pMode = DomainMode }) "back to the domain"
  , el "h2" [] "Drawn"
  , el "div" [] (intercalate "<br>" [ esc (inlineTex (texMat x ++ " = " ++ texWord x)) | x <- pMats p ])
  ]
  where
    m = last (pMats p)
    (pa, pb, pc, pd) = entries m
    box k v = "<input class=\"ent\" name=\"" ++ k ++ "\" value=\"" ++ show v ++ "\">"
    keep = if pCopy p then pMats p else init (pMats p)
    carriedTri = [ ("mode", "tri") ] ++ [ ("mats", showMats keep) | not (null keep) ] ++ [ ("copy", "1") | pCopy p ]
                 ++ [ (k, v) | (k, v) <- carried p ] ++ [ (k, v) | (k, v) <- parseQuery (toQuery p), k `elem` ["scale", "cx"] ]   -- the explorer keeps its view
    next x = p { pMats = keep ++ [x] }
    rights = [ ("MT", genT), ("MT⁻¹", genTinv), ("MS", genS), ("MR", genR) ]
    lefts  = [ ("TM", genT), ("T⁻¹M", genTinv), ("SM", genS), ("RM", genR) ]
    goto q = case q of
      Fin x -> p { pCx = x }
      Inf   -> p { pCx = fromInteger pb }
