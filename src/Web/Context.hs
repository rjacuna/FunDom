-- | What the page is rendered from: the parameters, the group they name
-- (or why there is none), and — in the browsing mode — the choices the
-- tables offer at the current position.
module Web.Context (Context(..)) where

import Modular.CP (Summary)
import Modular.Group (Subgroup)
import Web.Params

data Context = Context
  { cxParams  :: Params
  , cxGroup   :: Either String Subgroup
  , cxLevels  :: [Int]        -- ^ mode 2: levels with a group of the chosen genus
  , cxIndices :: [Int]        -- ^ mode 2: indices at the chosen genus and level
  , cxClass   :: [Summary]    -- ^ mode 2: the groups at the chosen genus, level and index
  }
