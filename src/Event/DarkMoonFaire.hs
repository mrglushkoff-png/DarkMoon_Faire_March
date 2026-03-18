module Event.DarkmoonFaire
  ( darkmoonPayloads
  ) where

import Types

g :: Int -> Reward
g n = zeroReward { rewardGold = n }

xp :: Int -> Reward
xp n = zeroReward { rewardExp = n }

minorStar :: Reward
minorStar = zeroReward { rewardMinorStars = 1 }

majorStar :: Reward
majorStar = zeroReward { rewardMajorStars = 1 }

ae :: Int -> Reward
ae n = zeroReward { rewardArclightEnergy = n }

epicCore :: Reward
epicCore = zeroReward { rewardEpicCores = 1 }

payload :: Int -> Reward -> Bool -> NodePayload
payload c r claimed =
  NodePayload
    { payloadCost    = c
    , payloadReward  = r
    , payloadClaimed = claimed
    }

darkmoonPayloads :: [NodePayload]
darkmoonPayloads =
  [ payload   400 (g   50) False
  , payload   200 (xp 1500) False
  , payload   200 (xp 1500) False
  , payload   400 (g   50) False

  , payload   300 (xp 1500) False
  , payload   900 (xp 3000) False
  , payload   300 (xp 1500) False

  , payload  1800 (ae  500) False
  , payload  1100 (g   150) False
  , payload  1100 (xp 3000) False
  , payload  3500 (g   200) False

  , payload  2200 minorStar False
  , payload  1500 (g   150) False
  , payload  1800 (xp 3000) False

  , payload 30000 epicCore  False
  , payload  3000 minorStar False
  , payload 15000 (xp 25000) False
  , payload  3500 majorStar False

  , payload  2000 (xp 3000) False
  , payload  6000 (g   200) False
  , payload  2000 (xp 3000) False

  , payload  6000 (ae 1000) False
  , payload  2200 (xp 3000) False
  , payload  2200 (xp 3000) False
  , payload  4000 majorStar False

  , payload 12000 (g   300) False
  , payload 18000 (ae 2500) False
  , payload 12000 (g   300) False
  ]