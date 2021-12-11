module AoC.Challenge.Day11
  ( day11a
  , day11b
  ) where

import           AoC.Solution
import           AoC.Util                       ( Point
                                                , allNeighbours
                                                , getFreqs
                                                , parseMap
                                                )
import           Data.Foldable                  ( find
                                                , toList
                                                )
import           Data.List                      ( scanl'
                                                , unfoldr
                                                )

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Linear.V2                      ( V2(..) )
import           Text.Read                      ( readEither )

type EnergyMap = Map Point Int
type FlashSet = Set Point

-- Run a single step.
-- Add one to all energy values, and the run all the flashes.
-- Returns the set of all flashed octopuses and the new energy map.
step :: EnergyMap -> (FlashSet, EnergyMap)
step e =
  let (f, e') = flashAll . fmap (+ 1) $ e
  in  (f, M.union e' (M.fromSet (const 0) f))

-- Run all the possible flashes at this state.
-- Returns the set of flashed octopuses, and updated energy map (that
-- doesn't include octpuses that flashed).
flashAll :: EnergyMap -> (FlashSet, EnergyMap)
flashAll = go S.empty
 where
  go :: FlashSet -> EnergyMap -> (FlashSet, EnergyMap)
  go f e =
    let (f', e') = flash e
    in  if S.null f' then (f, e') else go (S.union f f') e'

-- Run run flashes that are ready, and update the energy map.
-- The new energy map does not contain values for octpuses that flashed so
-- they don't flash again this cycle.
flash :: EnergyMap -> (FlashSet, EnergyMap)
flash m =
  let (ready, notReady) = M.partition (> 9) m
      neighbourFlashes  = getFreqs $ allNeighbours =<< M.keys ready
  in  ( M.keysSet ready
      , M.restrictKeys (M.unionWith (+) m neighbourFlashes) (M.keysSet notReady)
      )

day11a :: Solution (Map Point Int) Int
day11a = Solution
  { sParse = parseMap
  , sShow  = show
  , sSolve = Right . sum . fmap S.size . take 100 . unfoldr (Just . step)
  }

day11b :: Solution (Map Point Int) Int
day11b = Solution
  { sParse = parseMap
  , sShow  = show
  , sSolve = \m ->
               Right
                 . fst
                 . fromJust
                 . find ((M.keysSet m ==) . snd)
                 . zip [1 ..]
                 . unfoldr (Just . step)
                 $ m
  }
