{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Blackjack.Game
  ( GameSize (..),
    KnownGameSize (..),
    MaxCards,
  )
where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (Nat)

class KnownGameSize (gs :: GameSize) where
  maxCardsVal :: Proxy gs -> Int

instance KnownGameSize 'SingleDeck where maxCardsVal _ = 11

instance KnownGameSize 'DoubleDeck where maxCardsVal _ = 14

instance KnownGameSize 'SixDeck where maxCardsVal _ = 21

instance KnownGameSize 'EightDeck where maxCardsVal _ = 21

data GameSize = SingleDeck | DoubleDeck | SixDeck | EightDeck

type family MaxCards (gs :: GameSize) :: Nat where
  MaxCards 'SingleDeck = 11
  MaxCards 'DoubleDeck = 14
  MaxCards 'SixDeck = 21
  MaxCards 'EightDeck = 21
