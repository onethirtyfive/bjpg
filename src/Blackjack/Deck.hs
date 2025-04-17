{-# LANGUAGE GADTs #-}

module Blackjack.Deck (nDecks, Deck (..)) where

import Blackjack.Card

data Deck where
  Deck :: [Card] -> Deck
  deriving (Show, Eq)

nDecks :: Int -> Deck
nDecks n = Deck $ concat (replicate n normalDeck)
  where
    normalDeck = [Card _rank _suit | _rank <- [Two .. Ace], _suit <- [Hearts .. Spades]]
