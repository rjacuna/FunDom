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
  ( renderPage, renderSvgOnly, sceneFor, renderLevel
  ) where

import Data.Array ((!))
import Data.List (intercalate, sortOn)
import Data.Ratio
import Flint.Ball
import Flint.SL2
import Flint.Z
import Modular.CP (Summary(..), prettyName)
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
    layers = tessellation ++ tiling
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
                [ Layer (colourCss (pC2 p) 0) (colourCss (pC2 p) 0) 0.4 1 True (modularTiles precD dv 1.5 6000) | pBg p ]
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

renderSvgOnly :: Context -> String
renderSvgOnly cx = case sceneFor cx of
  Right sc -> renderSvg sc
  Left err -> "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"400\" height=\"60\"><text x=\"10\" y=\"30\">" ++ esc err ++ "</text></svg>"

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

renderPage :: Context -> String
renderPage cx = unlines
  [ "<!doctype html>"
  , "<html lang=\"en\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
  , "<title>Fundamental domains</title><style>" ++ css ++ "</style>"
  , katexHead
  , "</head><body>"
  , "<header><h1>Fundamental domains of congruence subgroups of SL₂(ℤ)</h1><span>after Helena A. Verrill's Fundamental Domain Drawer · PSL₂(ℤ) in FLINT · exact arcs · arb vertices</span></header>"
  , "<div class=\"layout\">"
  , el "div" [("class", "panel")] (leftPanel cx p)
  , el "div" [("id", "plot")] (either errorBox renderSvg scene)
  , el "div" [("class", "panel")] (rightPanel cx p built)
  , "</div></body></html>"
  ]
  where
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
  | (k, label) <- [(1, "1 · families"), (2, "2 · tables"), (3, "3 · generators")] ]

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
      [ el "label" [] (select "g1" [ (typeCode t, typeLabel t "N") | t <- allTypes ] (typeCode (pG1 p))
                       ++ " N <input type=\"number\" name=\"n\" min=\"1\" value=\"" ++ show (pN p) ++ "\">")
      , el "label" [] ("∩ " ++ select "g2" [ (typeCode t, typeLabel t "M") | t <- allTypes ] (typeCode (pG2 p))
                       ++ " M <input type=\"number\" name=\"m\" min=\"1\" value=\"" ++ show (pM p) ++ "\">")
      , el "label" [] ("scale <input class=\"rat\" name=\"scale\" value=\"" ++ esc (showRat (pScale p)) ++ "\"> px per unit")
      , el "label" [] ("centre <input class=\"rat\" name=\"cx\" value=\"" ++ esc (showRat (pCx p)) ++ "\">")
      , el "label" [] ("fill " ++ select "fill" [ (c', c') | c' <- colourNames ] (pFill p))
      , el "label" [] ("outline " ++ select "outline" [ (c', c') | c' <- colourNames ] (pOutline p))
      , concat [ hidden k v | (k, v) <- carried p, k `notElem` ["scale", "cx", "fill", "outline"] ]
      , "<button type=\"submit\">Draw</button>"
      ]
  , el "p" [] $ "N " ++ btn (href (fresh p) { pN = max 1 (pN p - 1) }) "−" ++ btn (href (fresh p) { pN = pN p + 1 }) "+"
             ++ "  M " ++ btn (href (fresh p) { pM = max 1 (pM p - 1) }) "−" ++ btn (href (fresh p) { pM = pM p + 1 }) "+"
  ]

-- | Mode 2: genus → level → index, from the tables, as three dropdowns.
-- Each one submits the form when changed (through @requestSubmit@, so the
-- single-page build sees it as a submit); the button is for browsers
-- without scripts.
browseControls :: Context -> Params -> String
browseControls cx p = concat
  [ el "h2" [] "Cummins–Pauli tables"
  , el "p" [("class", "muted")] "All congruence subgroups of PSL₂(ℤ) of genus ≤ 24, up to conjugacy. Pick a genus, a level and an index; the groups in that class are listed on the right."
  , el "form" [("method", "get"), ("action", "")] $ concat
      [ hidden "app" "2"
      , el "label" [] ("genus " ++ dropdown "gen" [ (show g, show g) | g <- [0 .. 24 :: Int] ] (fmap show (pGenus p)) "genus…")
      , el "label" [] ("level " ++ dropdown "lev" [ (show l, show l) | l <- cxLevels cx ] (fmap show (pLevel p)) (if null (cxLevels cx) then "(choose a genus)" else "level…"))
      , el "label" [] ("index " ++ dropdown "idx" [ (show i, show i) | i <- cxIndices cx ] (fmap show (pIndex p)) (if null (cxIndices cx) then "(choose a level)" else "index…"))
      , concat [ hidden k v | (k, v) <- carried p ]
      , "<button type=\"submit\">Show</button>"
      ]
  , el "h2" [] "Or by name"
  , el "form" [("method", "get"), ("action", "")] $ concat
      [ hidden "app" "2"
      , el "label" [] ("<input class=\"ent\" name=\"db\" placeholder=\"11A1\" value=\"" ++ esc (maybe "" id (pDb p)) ++ "\"> <button type=\"submit\">Load</button>")
      , concat [ hidden k v | (k, v) <- carried p ]
      ]
  ]

-- | A select that submits its form on change, with a placeholder when
-- nothing is chosen yet.
dropdown :: String -> [(String, String)] -> Maybe String -> String -> String
dropdown name opts cur placeholder =
  el "select" [("name", name), ("onchange", "this.form.requestSubmit()")] $
    (case cur of
       Just _  -> ""
       Nothing -> "<option value=\"\" selected disabled>" ++ esc placeholder ++ "</option>")
    ++ concat [ "<option value=\"" ++ esc v ++ "\"" ++ (if Just v == cur then " selected" else "") ++ ">" ++ esc label ++ "</option>" | (v, label) <- opts ]

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
      [ btn (href (fresh p) { pApp = 3, pGens = g }) (inlineTex label) | (label, g) <- examples ]
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
      Disk -> el "p" [] (toggle (pBg p) (href p { pBg = not (pBg p) }) "modular tessellation"
                         ++ toggle (pTile p) (href p { pTile = not (pTile p) }) "tile by Γ")
              ++ el "form" [("method", "get"), ("action", "")] (concat
                   [ el "label" [] ("tessellation " ++ select "c1" [ (c', c') | c' <- colourNames ] (pC1 p) ++ " / " ++ select "c2" [ (c', c') | c' <- colourNames ] (pC2 p))
                   , el "label" [] ("translates " ++ select "fill" [ (c', c') | c' <- colourNames ] (pFill p) ++ " / " ++ select "fill2" [ (c', c') | c' <- colourNames ] (pFill2 p))
                   , concat [ hidden k v' | (k, v') <- parseQuery (toQuery p), k `notElem` ["c1", "c2", "fill", "fill2"] ]
                   , "<button type=\"submit\">Change Colors</button>" ])
  ]
  where
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

-- | State a GET form must carry, since a submit otherwise resets it.
carried :: Params -> [(String, String)]
carried p = [ (k, v) | (k, v) <- parseQuery (toQuery p)
            , k `notElem` ["sel", "mv", "n", "m", "g1", "g2", "db", "gens", "app", "gen", "lev", "idx", "mode", "mats", "copy"] ]

-- | A change of group starts over: no selection, no rearrangements, no
-- explorer, no table lookup.
fresh :: Params -> Params
fresh p = p { pSel = Nothing, pMoves = [], pMode = DomainMode, pDb = Nothing, pGenus = Nothing, pLevel = Nothing, pIndex = Nothing }

svgHref :: Params -> String
svgHref p = "svg?" ++ toQuery p

-- The right panel ---------------------------------------------------------------------

rightPanel :: Context -> Params -> Either String Built -> String
rightPanel cx p built = case pApp p of
  2 -> classSelector cx p ++ either (const "") (infoPanel p) built
  3 -> certificate cx p ++ either (const "") (infoPanel p) built
  _ -> case pMode p of
         TriMode    -> explorer p
         DomainMode -> either (const "") (infoPanel p) built

-- | Mode 2: the groups in the chosen class.
classSelector :: Context -> Params -> String
classSelector cx p = case (pGenus p, pLevel p, pIndex p) of
  (Just g, Just l, Just i) -> concat
    [ el "h2" [] ("Genus " ++ show g ++ ", level " ++ show l ++ ", index " ++ show i)
    , el "div" [("class", "class")] $ concat
        [ el "a" [("class", if pDb p == Just (smName sm) then "on" else ""), ("href", href p { pDb = Just (smName sm), pSel = Nothing, pMoves = [] })]
                 (esc (smName sm) ++ maybe "" (\s -> "  ·  " ++ esc (prettyName s)) (smSpecial sm)
                  ++ el "span" [("class", "muted")] ("  cusps " ++ unwords (map show (smCusps sm))))
        | sm <- cxClass cx ]
    , el "p" [("class", "muted")] "Within a genus and level the letter is a position in their sorted list, not an invariant; the record's generators are what define the group."
    ]
  _ -> el "p" [("class", "muted")] "Choose a genus, then a level, then an index."

-- | Mode 3: what the enumeration and Hsu's criterion found.
certificate :: Context -> Params -> String
certificate cx p = case cxGroup cx of
  Left err -> el "h2" [] "Certificate" ++ el "p" [("class", "warn")] (esc err)
  Right sg -> concat
    [ el "h2" [] "Certificate"
    , el "p" [] (either esc (\gs -> esc (inlineTex ("\\left\\langle " ++ intercalate ",\\ " (map texMat gs) ++ "\\right\\rangle"))) (parseGenerators (pGens p)))
    , case (sgVerdict sg, sgExpect sg) of
        (Just (lv, verdict), Just ex) -> concat
          [ el "table" [("class", "kv")] $ concat
              [ row "index" (show (exIndex ex))
              , row "generalised level" (show lv ++ "  (lcm of the cusp widths)")
              , row "cusp widths" (unwords (map show (exCusps ex)))
              , row "genus" (show (exGenus ex))
              ]
          , case verdict of
              Nothing  -> el "p" [("class", "ok")] ("Congruence: the group contains Γ(" ++ show lv ++ "). Hsu's relations for level " ++ show lv ++ " hold in the coset action.")
              Just why -> el "p" [("class", "warn")] ("Not congruence: Hsu's relation " ++ esc why ++ " fails in the coset action, so the group does not contain Γ(" ++ show lv ++ "), and by Wohlfahrt's theorem contains no Γ(N) at all.")
          ]
        _ -> ""
    , el "p" [("class", "muted")] "The coset action of PSL₂(ℤ) = ⟨S⟩ * ⟨R⟩ on the subgroup's cosets is found by tracing the generators as loops and closing the table (Todd–Coxeter); the domain below is drawn from that action."
    ]
  where
    row k v = el "tr" [] (el "td" [] (esc k) ++ el "td" [] (esc v))

-- | The invariants and the selected triangle.
infoPanel :: Params -> Built -> String
infoPanel p (Built dom inf) = concat
  [ el "h2" [] (esc (subgroupName sg))
  , el "table" [("class", "kv")] $ concat
      [ row "index" (show n ++ if containsMinusOne sg then "" else " (projective; " ++ show (2 * n) ++ " in SL₂(ℤ), as −I ∉ Γ)")
      , row "genus" (show (iGenus inf))
      , row "cusps" (show (length (iCusps inf)))
      , row "elliptic" ("order 2: " ++ show (iE2 inf) ++ ", order 3: " ++ show (iE3 inf))
      ]
  , if iEuler inf then "" else el "p" [("class", "warn")] "Euler characteristic is not integral — the table is inconsistent."
  , checks
  , el "h2" [] "Cusps and widths"
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
    row k v = el "tr" [] (el "td" [] (esc k) ++ el "td" [] (esc v))
    -- what the source recorded against what was computed from the domain
    checks = case sgExpect sg of
      Nothing -> ""
      Just ex ->
        let ours   = sortDesc (map cWidth (iCusps inf))
            theirs = sortDesc (exCusps ex)
            hOK    = case sgOrderH sg of
                       Just h  -> h * n == sl2Order (sgLevel sg)
                       Nothing -> True
            line k a b = el "tr" [] (el "td" [] (esc k) ++ el "td" [] (esc a) ++ el "td" [] (esc b)
                                     ++ el "td" [] (if a == b then "✓" else el "span" [("class", "warn")] "✗"))
        in el "h2" [] ("Against " ++ esc (exSource ex))
           ++ el "table" [("class", "kv")] (concat
                [ el "tr" [] (el "td" [] "" ++ el "td" [] "domain" ++ el "td" [] (esc (exSource ex)) ++ el "td" [] "")
                , line "index" (show n) (show (exIndex ex))
                , line "genus" (show (iGenus inf)) (show (exGenus ex))
                , line "cusp widths" (unwords (map show ours)) (unwords (map show theirs))
                , line "e₂" (show (iE2 inf)) (show (exE2 ex))
                , line "e₃" (show (iE3 inf)) (show (exE3 ex))
                ])
           ++ (case sgOrderH sg of
                 Just h -> el "p" [("class", "muted")] ("level " ++ show (sgLevel sg) ++ "; |⟨generators, −I⟩| = " ++ show h ++ " in SL₂(ℤ/" ++ show (sgLevel sg) ++ "), of order "
                             ++ show (sl2Order (sgLevel sg)) ++ (if hOK then " ✓" else " — does not match the index ✗"))
                           ++ el "p" [] (el "a" [("href", "csg?level=" ++ show (sgLevel sg))] ("all groups of level " ++ show (sgLevel sg)))
                           ++ el "p" [("class", "muted")] "The tables list groups up to conjugacy in PGL₂(ℤ); the generators define one representative, which need not be the classical group of the same name."
                 Nothing -> "")
    sortDesc = reverse . sortOn id
    goto q sc = case q of
      Fin x -> p { pCx = x, pScale = sc }
      Inf   -> p { pCx = 0, pScale = sc }
    selected i = concat
      [ el "h2" [] ("Triangle #" ++ show i)
      , el "p" [] (esc (inlineTex (texMat g ++ " = " ++ texWord g)))
      , el "table" [("class", "kv")] $ concat $
          row "cusp" (showQI (cusp g)) :
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
                 ++ [ (k, v) | (k, v) <- carried p ]
    next x = p { pMats = keep ++ [x] }
    rights = [ ("MT", genT), ("MT⁻¹", genTinv), ("MS", genS), ("MR", genR) ]
    lefts  = [ ("TM", genT), ("T⁻¹M", genTinv), ("SM", genS), ("RM", genR) ]
    goto q = case q of
      Fin x -> p { pCx = x }
      Inf   -> p { pCx = fromInteger pb }

-- | The listing of every group of one level in the tables.
renderLevel :: Int -> [Summary] -> String
renderLevel lv sms = unlines
  [ "<!doctype html>"
  , "<html lang=\"en\"><head><meta charset=\"utf-8\"><title>Congruence subgroups of level " ++ show lv ++ "</title><style>" ++ css ++ "</style></head><body>"
  , "<header><h1>Cummins–Pauli: congruence subgroups of level " ++ show lv ++ "</h1><span>" ++ show (length sms) ++ " groups of genus ≤ 24</span></header>"
  , el "div" [("class", "layout"), ("style", "grid-template-columns:1fr")] $ el "div" [("class", "panel")] $
      el "p" [] (el "a" [("href", "?app=2")] "← back") ++
      el "table" [("class", "kv")] (concat $
        el "tr" [] (concatMap (el "th" []) ["name", "classical", "index", "genus", "cusp widths", ""]) :
        [ el "tr" [] $ concat
            [ el "td" [] (el "a" [("href", "?app=2&db=" ++ esc (smName sm))] (esc (smName sm)))
            , el "td" [] (esc (maybe "" prettyName (smSpecial sm)))
            , el "td" [] (show (smIndex sm))
            , el "td" [] (show (smGenus sm))
            , el "td" [] (unwords (map show (smCusps sm)))
            , el "td" [] (el "a" [("href", "?app=2&db=" ++ esc (smName sm) ++ "&view=disk")] "disk")
            ]
        | sm <- sms ])
  , "</div></body></html>"
  ]
