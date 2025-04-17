{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Blackjack.Category
  ( HandCategory (..),
    Upcard,
    categoryFromHand,
  )
where

import Blackjack.Hand (Hand, HandTotalDescriptor (..), handDescriptor)
import Blackjack.Card (Rank)

-- | The dealer’s up‑card is just the card rank.
type Upcard = Rank

-- | What the basic‑strategy chart keys on.
data HandCategory
  = Hard Int -- hard total 5‑20   (we never look at hard 4 or below)
  | Soft Int -- soft totals 13‑20
  | Pair Rank -- one of the first two cards duplicated
  deriving (Eq, Show)

-- | Utility – turn a @Hand gs n@ into the category (+ numeric total if needed)
categoryFromHand :: Hand gs n -> Maybe HandCategory
categoryFromHand h =
  case handDescriptor h of
    HardDescriptor n
      | n >= 5 && n <= 20 -> Just (Hard n)
      | otherwise -> Nothing
    SoftDescriptor lo _hi
      | lo >= 3 && lo <= 10 -> Just (Soft (lo + 10)) -- 3→13, … 10→20
      | otherwise -> Nothing
    _ ->
      -- busts or <5 are irrelevant for strategy
      Nothing
