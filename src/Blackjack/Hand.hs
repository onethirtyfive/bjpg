{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Blackjack.Hand
  ( Vec (..),
    Hand,
    mkHand,
    snoc,
    Dealable (..),
    AnyHand (..),
    PlayableHand (..),
    dealAny,
    mkPlayable,
    toListVec,
    HandTotal (..),
    HandTotalDescriptor (..),
    handTotal,
    fromHandTotal,
    handDescriptor,
  )
where

import Blackjack.Card (Card (..), Rank (..), realValue)
import Blackjack.Game (GameSize, KnownGameSize (..), MaxCards)
import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), Nat, TypeError, type (+))
import qualified GHC.TypeLits as TL

type family If (cond :: Bool) (t :: k) (f :: k) :: k where
  If 'True t _ = t
  If 'False _ f = f

type family AssertMax (max :: Nat) (n :: Nat) :: Constraint where
  AssertMax max n =
    If
      (n TL.<=? max)
      (() :: Constraint)
      ( TypeError
          ( 'Text "Hand length "
              ':<>: 'ShowType n
              ':<>: 'Text " exceeds maximum of "
              ':<>: 'ShowType max
          )
      )

data Vec (n :: Nat) a where
  VNil :: Vec 0 a
  (:#) :: a -> Vec n a -> Vec (n + 1) a

infixr 5 :#

toListVec :: Vec n a -> [a]
toListVec VNil = []
toListVec (x :# xs) = x : toListVec xs

snoc :: Vec n a -> a -> Vec (n + 1) a
snoc VNil x = x :# VNil
snoc (y :# ys) x = y :# snoc ys x

newtype Hand (gs :: GameSize) (n :: Nat) = Hand (Vec n Card)

mkHand :: (AssertMax (MaxCards gs) n) => Vec n Card -> Hand gs n
mkHand = Hand

class Dealable h where
  type NextHand h
  deal :: h -> Card -> NextHand h

instance
  (AssertMax (MaxCards gs) (n + 1)) =>
  Dealable (Hand gs n)
  where
  type NextHand (Hand gs n) = Hand gs (n + 1)
  deal (Hand v) c = mkHand (snoc v c)

-- | Existential wrapper for any hand up to the max size
data AnyHand (gs :: GameSize) where
  AnyHand :: (AssertMax (MaxCards gs) n) => Hand gs n -> AnyHand gs

-- | Existential wrapper for hands that can still be hit once more
data PlayableHand (gs :: GameSize) where
  PlayableHand ::
    (AssertMax (MaxCards gs) (n + 1)) =>
    Hand gs n ->
    PlayableHand gs

dealAny :: PlayableHand gs -> Card -> AnyHand gs
dealAny (PlayableHand h) c = AnyHand (deal h c)

mkPlayable ::
  forall gs n.
  ( KnownGameSize gs,
    AssertMax (MaxCards gs) (n + 1)
  ) =>
  Hand gs n ->
  Maybe (PlayableHand gs)
mkPlayable h@(Hand v)
  | length (toListVec v) < maxCardsVal (Proxy :: Proxy gs) =
      Just (PlayableHand h)
  | otherwise =
      Nothing

newtype Hard a = Lo a deriving (Show, Eq)

newtype Soft a = Hi a deriving (Show, Eq)

data HandTotal a
  = HardTotal a
  | SoftTotal (Hard a, Soft a)
  deriving (Eq)

instance (Show a) => Show (HandTotal a) where
  show (HardTotal n) = "Hard " ++ show n
  show (SoftTotal (Lo l, Hi h)) = "Soft (" ++ show l ++ ", " ++ show h ++ ")"

data HandTotalDescriptor
  = HardDescriptor Int
  | SoftDescriptor Int Int
  | BustDescriptor Int
  deriving (Eq, Show)

totalFromList :: [Card] -> HandTotal Int
totalFromList cs =
  let vals = map realValue cs
      s = sum vals
      softOK = any ((== Ace) . rank) cs && s <= 21
   in if softOK
        then SoftTotal (Lo (s - 10), Hi s)
        else HardTotal s

handTotal :: Vec n Card -> HandTotal Int
handTotal = totalFromList . toListVec

fromHandTotal :: HandTotal Int -> HandTotalDescriptor
fromHandTotal (HardTotal n)
  | n <= 21 = HardDescriptor n
  | otherwise = BustDescriptor n
fromHandTotal (SoftTotal (Lo l, Hi h)) = SoftDescriptor l h

handDescriptor :: Hand gs n -> HandTotalDescriptor
handDescriptor (Hand v) = fromHandTotal (handTotal v)
