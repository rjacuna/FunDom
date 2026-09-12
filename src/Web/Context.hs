-- FunDom — fundamental domains of congruence subgroups of SL₂(ℤ).
-- Copyright (C) 2026 RJ Acuña. Mode 1 derives from Helena A. Verrill's
-- FunDomain (Copyright (C) 2001, GPL-2.0-or-later); see java/ and README.md.
-- SPDX-License-Identifier: GPL-3.0-or-later
-- | What the page is rendered from: the parameters, the group they name
-- (or why there is none), and — in the browsing mode — the groups matching
-- the chosen filters and what the dropdowns may offer.
module Web.Context (Context(..)) where

import Modular.CP (Options, Record, Summary)
import Modular.Group (Subgroup)
import Web.Params

data Context = Context
  { cxParams     :: Params
  , cxGroup      :: Either String Subgroup
  , cxRecord     :: Maybe Record   -- ^ mode 2: the chosen entry of the tables
  , cxOptions    :: Options        -- ^ mode 2: values of genus, level, index consistent with the other filters
  , cxClass      :: [Summary]      -- ^ mode 2: the groups matching every chosen filter
  , cxCandidates :: [Record]       -- ^ modes 1 and 3: table entries with the group's genus, level, index and cusp widths
  , cxNames      :: [(String, String)] -- ^ classical names of the entries the panel may link to
  }
