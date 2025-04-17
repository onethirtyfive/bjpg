{-# LANGUAGE DeriveGeneric #-}

module Blackjack.Action
  ( Action (..),
  )
where

import GHC.Generics (Generic)

-- | The final *allowed* move after we’ve checked the table rules.
data Action
  = Hit
  | Stand
  | Double -- double down (exactly one more card)
  | Split -- split the pair
  | Surrender -- lose half, keep half
  | DoubleOrStand
  | DoubleOrHit
  deriving (Eq, Show, Generic)
