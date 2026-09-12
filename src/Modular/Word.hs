-- | PSL₂(ℤ) as the free product C₂ * C₃ = ⟨S⟩ * ⟨R⟩, with S = [0 −1; 1 0]
-- and R = S·T = [0 −1; 1 1]. Every element is a unique reduced word in
-- S, R, R⁻¹ with no S·S, R·R⁻¹ or R·R·R; this module goes from matrices
-- to words (Euclid's algorithm on the first column) and back.
module Modular.Word
  ( Letter(..), GWord, reduceW, wordOf, wordMat, invWord, showWord
  ) where

import Flint.SL2

data Letter = LS | LR | LRi
  deriving (Eq, Ord, Show)

-- | A product, read left to right.
type GWord = [Letter]

letterMat :: Letter -> SL2
letterMat LS  = genS
letterMat LR  = genR
letterMat LRi = inv genR

wordMat :: GWord -> SL2
wordMat = foldl mul identity . map letterMat

invLetter :: Letter -> Letter
invLetter LS  = LS
invLetter LR  = LRi
invLetter LRi = LR

invWord :: GWord -> GWord
invWord = reverse . map invLetter

showWord :: GWord -> String
showWord [] = "1"
showWord w  = concatMap sh w
  where
    sh LS  = "S"
    sh LR  = "R"
    sh LRi = "R⁻¹"

-- | The reduced form: S² = 1, R³ = 1, so R·R = R⁻¹ and R·R⁻¹ = 1.
reduceW :: GWord -> GWord
reduceW = reverse . foldl push []
  where
    push (LS  : st) LS  = st
    push (LR  : st) LRi = st
    push (LRi : st) LR  = st
    push (LR  : st) LR  = LRi : st
    push (LRi : st) LRi = LR  : st
    push st l           = l : st

-- | The reduced word of a matrix. With T = S·R: peel off @T^q·S@ on the
-- left while the bottom-left entry is nonzero (Euclid on the first
-- column), and what is left is a power of T.
wordOf :: SL2 -> GWord
wordOf g = reduceW (go (entries g))
  where
    go (a, b, c, d)
      | c == 0    = tPow (if a == 1 then b else negate b)
      | otherwise =
          let q = a `div` c
          in tPow q ++ [LS] ++ go (negate c, negate d, a - q * c, b - q * d)
    tPow k
      | k >= 0    = concat (replicate (fromInteger k) [LS, LR])
      | otherwise = concat (replicate (fromInteger (negate k)) [LRi, LS])
