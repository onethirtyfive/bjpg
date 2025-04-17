{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Blackjack.Strategy
  ( chooseBasicAction,
    KnownRules (..),
  )
where

import Blackjack.Action (Action (..))
import Blackjack.Card (Rank (..))
import Blackjack.Category (HandCategory (..), Upcard)
import Blackjack.Rules
import Data.Proxy (Proxy (..))

-- | Pick the correct basic‑strategy move **after** masking out illegal
-- doubles / surrenders under the current rule set.
chooseBasicAction ::
  forall cfg.
  (RulePolicies cfg) =>
  Proxy cfg ->
  -- | player hand bucket
  HandCategory ->
  -- | dealer up‑card
  Upcard ->
  Action
chooseBasicAction pcfg cat up =
  legalise (base cat up)
  where
    -- ------------ 1. unified chart (assuming everything is allowed) -----
    base :: HandCategory -> Rank -> Action
    -- Surrender slice  ----------------------------------------------
    base (Hard 16) u | u >= Nine = surrenderOr (Hard 16) -- 9,10,A
    base (Hard 15) Ten = surrenderOr (Hard 15)
    -- Pair section --------------------------------------------------
    base (Pair Ace) _ = Split
    base (Pair Ten) _ = Stand
    base (Pair Nine) u
      | u == Seven = Stand
      | u >= Two && u <= Nine = Split
      | otherwise = Stand
    base (Pair Eight) _ = Split
    base (Pair Seven) u
      | u >= Two && u <= Seven = Split
      | otherwise = Hit
    base (Pair Six) u
      | u >= Two && u <= Six = Split
      | otherwise = Hit
    base (Pair Five) u
      | u >= Two && u <= Nine = Double
      | otherwise = Hit
    base (Pair Four) u
      | u == Five || u == Six = Split
      | otherwise = Hit
    base (Pair Three) u
      | u >= Two && u <= Seven = Split
      | otherwise = Hit
    base (Pair Two) u
      | u >= Two && u <= Seven = Split
      | otherwise = Hit
    -- Soft totals ---------------------------------------------------
    base (Soft 20) _ = Stand
    base (Soft 19) Six = DoubleOrStand
    base (Soft 19) _ = Stand
    base (Soft 18) u
      | u >= Two && u <= Six = DoubleOrStand
      | u >= Nine || u == Ace = Hit -- 9,T,A → hit
      | otherwise = Stand
    base (Soft 17) u
      | u >= Three && u <= Six = DoubleOrHit
      | otherwise = Hit
    base (Soft 16) u
      | u >= Four && u <= Six = DoubleOrHit
      | otherwise = Hit
    base (Soft 15) u
      | u >= Four && u <= Six = DoubleOrHit
      | otherwise = Hit
    base (Soft 14) u
      | u == Five || u == Six = DoubleOrHit
      | otherwise = Hit
    base (Soft 13) u
      | u == Five || u == Six = DoubleOrHit
      | otherwise = Hit
    -- Hard totals ---------------------------------------------------
    base (Hard n) _
      | n >= 17 = Stand
    base (Hard 16) u
      | u >= Two && u <= Six = Stand
      | otherwise = Hit
    base (Hard 15) u
      | u >= Two && u <= Six = Stand
      | otherwise = Hit
    base (Hard 14) u
      | u >= Two && u <= Six = Stand
      | otherwise = Hit
    base (Hard 13) u
      | u >= Two && u <= Six = Stand
      | otherwise = Hit
    base (Hard 12) u
      | u >= Four && u <= Six = Stand
      | otherwise = Hit
    base (Hard 11) _ = Double
    base (Hard 10) u
      | u >= Two && u <= Nine = Double
      | otherwise = Hit
    base (Hard 9) u
      | u >= Three && u <= Six = Double
      | otherwise = Hit
    base (Hard _) _ = Hit -- hard 8 or below
    -- fallback when surrender is not taken
    surrenderOr h = if surrenderAllowed then Surrender else base h up

    -- ------------ 2. rule‑masking layer -------------------------------
    legalise :: Action -> Action
    legalise Double
      | doubleAllowed = Double
      | otherwise = Hit
    legalise DoubleOrStand
      | doubleAllowed = Double
      | otherwise = Stand
    legalise DoubleOrHit
      | doubleAllowed = Double
      | otherwise = Hit
    legalise Split = Split -- splitting legality handled elsewhere
    legalise x = x -- Hit, Stand, Surrender unchanged

    -- booleans driven by the phantom cfg ------------------------------
    surrenderAllowed :: Bool
    surrenderAllowed =
      case surrenderFlag of
        Early -> True
        Late -> True
        NoSurrender -> False

    doubleAllowed :: Bool
    doubleAllowed =
      case catTotal of
        Nothing -> False
        Just t -> canDouble (Proxy @(DoubleFlag cfg)) t
      where
        catTotal = case cat of
          Hard n -> Just n
          Soft n -> Just n
          Pair Five -> Just 10
          Pair Four -> Just 8
          Pair _ -> Nothing -- pairs that never double
    surrenderFlag = surrender (rulesVal pcfg)
