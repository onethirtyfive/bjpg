{-# LANGUAGE GADTs #-}

module Blackjack.Card (cardFamily, realValue, valuate, CardFamily (..), Suit (..), Rank (..), Card (..)) where

data CardFamily a = CFAce | CFFace | CFPip a
  deriving (Show, Eq)

cardFamily :: Card -> CardFamily Int
cardFamily (Card r _) = case r of
  Two -> CFPip 2
  Three -> CFPip 3
  Four -> CFPip 4
  Five -> CFPip 5
  Six -> CFPip 6
  Seven -> CFPip 7
  Eight -> CFPip 8
  Nine -> CFPip 9
  Ten -> CFPip 10
  Jack -> CFFace
  Queen -> CFFace
  King -> CFFace
  Ace -> CFAce

valuate :: CardFamily Int -> Int
valuate CFAce = 11
valuate CFFace = 10
valuate (CFPip n) = n

realValue :: Card -> Int
realValue = valuate . cardFamily

data Suit = Hearts | Diamonds | Clubs | Spades
  deriving (Show, Eq, Enum)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Ord)

data Card = Card {rank :: Rank, suit :: Suit}
  deriving (Show, Eq)
