{-# LANGUAGE DataKinds #-}

module Main where

import Blackjack.Card (Card (..))
import Blackjack.Deck (Deck (..), nDecks)
import Blackjack.Game (GameSize (..))
import Blackjack.Hand
import Control.Monad (forM)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import System.Random (randomRIO)

-- Fisher–Yates shuffle
shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray _ = newListArray (1, n)

drawCards :: Int -> Deck -> Either String ([Card], Deck)
drawCards k (Deck cs) = go k cs []
  where
    go 0 rest acc = Right (reverse acc, Deck rest)
    go _ [] _ = Left "The deck is empty."
    go m (c : cs') acc = go (m - 1) cs' (c : acc)

main :: IO ()
main = do
  let (Deck allCards) = nDecks 6 :: Deck
  shuffled <- shuffle allCards
  let deck0 = Deck shuffled

  -- case drawCards 11 deck0 of
  --   Left err -> putStrLn err
  --   Right ([c1, c2, c3, c4, c5, c6, c7, c8, c9, c10], deck1) -> do
  --     putStrLn $ "Initial cards: " ++ show [c1, c2, c3, c4, c5, c6, c7, c8, c9, c10]
  --     let initialHand = mkHand (c1 :# c2 :# c3 :# c4 :# c5 :# c6 :# c7 :# c8 :# c9 :# c10 :# VNil) :: Hand 'SingleDeck 10
  --     putStrLn $ "Initial total: " ++ show (handDescriptor initialHand)
  case drawCards 2 deck0 of
    Left err -> putStrLn err
    Right ([c1, c2], deck1) -> do
      putStrLn $ "Initial cards: " ++ show [c1, c2]
      let initialHand = mkHand (c1 :# c2 :# VNil) :: Hand 'SingleDeck 2
      putStrLn $ "Initial total: " ++ show (handDescriptor initialHand)

      case drawCards 1 deck1 of
        Left err -> putStrLn err
        Right ([c3], _) -> do
          putStrLn $ "Drew a third card: " ++ show c3

          let playable :: PlayableHand 'SingleDeck
              playable = PlayableHand initialHand

          case dealAny playable c3 of
            AnyHand newHand ->
              putStrLn $ "After hit: " ++ show (handDescriptor newHand)
        Right _ -> putStrLn "Unexpected draw size"
    Right _ -> putStrLn "Not enough cards drawn"
