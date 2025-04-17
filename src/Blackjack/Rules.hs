{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Blackjack.Rules
  ( -- * promoted kinds
    Decks (..),
    Pitch (..),
    Soft17Rule (..),
    DASRule (..),
    DoubleRule (..),
    SplitAces (..),
    SplitHands (..),
    Surrender (..),
    Payout (..),
    Pen (..),

    -- * bundled rule record
    Rules (..),

    -- * helpers: extractors + policy classes
    RulePolicies,
    DeckCount,
    FaceUpPlay,
    hitSoft17,
    allowDAS,
    canDouble,
    KnownRules (..),
    DoubleFlag,
  )
where

import Data.Kind (Constraint)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (ErrorMessage (..), Nat, TypeError)

----------------------------------------------------------------
-- 1. Enumerations  (promoted to kinds by DataKinds)
----------------------------------------------------------------

data Decks = D1 | D2 | D6 | D8 deriving (Show, Eq)

data Pitch = FaceUp | FaceDown deriving (Show, Eq) -- dealer’s dealing style

data Soft17Rule = StandSoft17 | HitSoft17 deriving (Show, Eq)

data DASRule = DAS | NoDAS deriving (Show, Eq)

data DoubleRule = DoubleAny | Double9_10 | Double9_11 | Double10_11 deriving (Show, Eq)

data SplitAces = NSA | NRSA | RSA deriving (Show, Eq) -- none / once / re‑split

data SplitHands = SP2 | SP3 | SP4 deriving (Show, Eq)

data Surrender = Early | Late | NoSurrender deriving (Show, Eq)

data Payout = P3_2 | P6_5 deriving (Show, Eq)

data Pen
  = PenCards Nat -- exact cut‑card position
  | PenFrac Nat Nat -- e.g. 5 of 6 decks = 5/6
  deriving (Show, Eq)

----------------------------------------------------------------
-- 2. Bundle them in one phantom record (kind = Rules)
----------------------------------------------------------------

data Rules = Rules
  { decks :: Decks,
    pitch :: Pitch, -- independent: lets you override the default
    soft17 :: Soft17Rule,
    das :: DASRule,
    doubling :: DoubleRule,
    splitAces :: SplitAces,
    splitHands :: SplitHands,
    surrender :: Surrender,
    payout :: Payout,
    pen :: Pen
  }
  deriving (Show, Eq)

----------------------------------------------------------------
-- 3. Type‑level accessors  (tiny record lenses)
----------------------------------------------------------------

type family DeckCount (cfg :: Rules) :: Decks where
  DeckCount ('Rules d _ _ _ _ _ _ _ _ _) = d

type family FaceUpPlay (cfg :: Rules) :: Pitch where
  FaceUpPlay ('Rules _ p _ _ _ _ _ _ _ _) = p

type family Soft17Flag (cfg :: Rules) :: Soft17Rule where
  Soft17Flag ('Rules _ _ s _ _ _ _ _ _ _) = s

type family DASFlag (cfg :: Rules) :: DASRule where
  DASFlag ('Rules _ _ _ d _ _ _ _ _ _) = d

type family DoubleFlag (cfg :: Rules) :: DoubleRule where
  DoubleFlag ('Rules _ _ _ _ dbl _ _ _ _ _) = dbl

-- …add more as/when you need them …

----------------------------------------------------------------
-- 4. Bridge each flag to term‑level behaviour
----------------------------------------------------------------

-- ①  soft‑17 policy
class KnownSoft17 (r :: Soft17Rule) where
  hitSoft17 :: Proxy r -> Bool

instance KnownSoft17 'StandSoft17 where hitSoft17 _ = False

instance KnownSoft17 'HitSoft17 where hitSoft17 _ = True

-- ②  DAS policy
class KnownDAS (d :: DASRule) where
  allowDAS :: Proxy d -> Bool

instance KnownDAS 'DAS where allowDAS _ = True

instance KnownDAS 'NoDAS where allowDAS _ = False

-- ③  doubling policy
class KnownDouble (dbl :: DoubleRule) where
  canDouble :: Proxy dbl -> Int -> Bool -- Int = starting hard/soft total

instance KnownDouble 'DoubleAny where canDouble _ _ = True

instance KnownDouble 'Double9_10 where canDouble _ t = t == 9 || t == 10

instance KnownDouble 'Double9_11 where canDouble _ t = t >= 9 && t <= 11

instance KnownDouble 'Double10_11 where canDouble _ t = t == 10 || t == 11

----------------------------------------------------------------
-- 5. Convenience constraint alias: “I know every policy I need”
----------------------------------------------------------------

type RulePolicies cfg =
  ( KnownSoft17 (Soft17Flag cfg),
    KnownDAS (DASFlag cfg),
    KnownDouble (DoubleFlag cfg),
    KnownRules cfg
  )

----------------------------------------------------------------
-- 6. Optional: compile‑time sanity checks / defaults
----------------------------------------------------------------
-- Face‑up 1‑ & 2‑deck, face‑down shoes unless the user overrides
type family PitchOK (d :: Decks) (p :: Pitch) :: Constraint where
  PitchOK 'D1 FaceUp = () -- 1‑deck pitch default OK
  PitchOK 'D2 FaceUp = ()
  PitchOK 'D6 FaceDown = ()
  PitchOK 'D8 FaceDown = ()
  PitchOK d p =
    TypeError
      ( 'Text "Inconsistent pitch: "
          ':<>: 'ShowType d
          ':<>: 'Text " normally uses "
          ':<>: 'ShowType (DefaultPitch d)
          ':<>: 'Text ", but you chose "
          ':<>: 'ShowType p
      )

type family DefaultPitch (d :: Decks) :: Pitch where
  DefaultPitch 'D1 = FaceUp
  DefaultPitch 'D2 = FaceUp
  DefaultPitch _ = FaceDown -- shoes

----------------------------------------------------------------
-- 7. Example aliases (“table presets”)
----------------------------------------------------------------

-- Classic six‑deck S17, DAS, double any two, RSA, SP4, late surrender, 3:2,
-- cut card at 5 decks (≈ 260 cards).
type Vegas6 =
  'Rules
    'D6
    FaceDown
    'StandSoft17
    'DAS
    'DoubleAny
    'RSA
    'SP4
    'Late
    'P3_2
    ('PenFrac 5 6)

-- Downtown single deck, H17, nDAS, double 10‑11, NRSA, SP2, no surrender, 6:5,
-- cut at 50 cards.
type DowntownSD =
  'Rules
    'D1
    FaceUp
    'HitSoft17
    'NoDAS
    'Double10_11
    'NRSA
    'SP2
    'NoSurrender
    'P6_5
    ('PenCards 50)

-- Compile‑time validation example:
--   :kind! PitchOK (DeckCount Vegas6) (FaceUpPlay Vegas6)  -- passes
--   :kind! PitchOK (DeckCount Vegas6) 'FaceUp              -- fails with nice error

class KnownRules (cfg :: Rules) where
  rulesVal :: Proxy cfg -> Rules
  showRules :: Proxy cfg -> String
  showRules = show . rulesVal

-- Then putStrLn (showRules (Proxy @Vegas6))
instance KnownRules Vegas6 where
  rulesVal _ =
    Rules
      { decks = D6,
        pitch = FaceDown,
        soft17 = StandSoft17,
        das = DAS,
        doubling = DoubleAny,
        splitAces = RSA,
        splitHands = SP4,
        surrender = Late,
        payout = P3_2,
        pen = PenFrac 5 6
      }

type family SplitOK (sa :: SplitAces) (sh :: SplitHands) :: Constraint where
  SplitOK 'NSA 'SP2 = ()
  SplitOK 'NRSA 'SP2 = ()
  SplitOK 'RSA sh = () -- any SPn is fine if RSA allowed
  SplitOK _ 'SP3 =
    TypeError ('Text "Cannot reach 3 hands when aces can’t be re‑split")

-- …and so on…

type family ValidateConfig (cfg :: Rules) :: Constraint where
  ValidateConfig ('Rules _ _ _ _ _ sa sh _ _ _) = SplitOK sa sh

-- instance FromJSON Rules
-- instance ToJSON   Rules
