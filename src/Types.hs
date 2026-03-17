module Types
  ( NodeId
  , Row
  , Reward(..)
  , zeroReward
  , addReward
  , Valuation(..)
  , defaultValuation
  , scoreReward
  , scoreLeftoverTickets
  , EventName(..)
  , InputMode(..)
  , Config(..)
  , NodePayload(..)
  , defaultPayload
  , Node(..)
  , Graph
  , ValidationError(..)
  , LogPhase(..)
  , LogEntry(..)
  ) where

type NodeId = String
type Row    = Int

data Reward = Reward
  { rewardGold           :: !Int
  , rewardExp            :: !Int
  , rewardMinorStars     :: !Int
  , rewardMajorStars     :: !Int
  , rewardArclightEnergy :: !Int
  , rewardRareCores      :: !Int
  } deriving (Eq, Show)

zeroReward :: Reward
zeroReward =
  Reward
    { rewardGold           = 0
    , rewardExp            = 0
    , rewardMinorStars     = 0
    , rewardMajorStars     = 0
    , rewardArclightEnergy = 0
    , rewardRareCores      = 0
    }

addReward :: Reward -> Reward -> Reward
addReward a b =
  Reward
    { rewardGold           = rewardGold a           + rewardGold b
    , rewardExp            = rewardExp a            + rewardExp b
    , rewardMinorStars     = rewardMinorStars a     + rewardMinorStars b
    , rewardMajorStars     = rewardMajorStars a     + rewardMajorStars b
    , rewardArclightEnergy = rewardArclightEnergy a + rewardArclightEnergy b
    , rewardRareCores      = rewardRareCores a      + rewardRareCores b
    }

data Valuation = Valuation
  { valueExp            :: !Double
  , valueMinorStar      :: !Double
  , valueMajorStar      :: !Double
  , valueArclightEnergy :: !Double
  , valueRareCore       :: !Double
  , valueLeftoverTicket :: !Double
  } deriving (Eq, Show)

defaultValuation :: Valuation
defaultValuation =
  Valuation
    { valueExp            = 100 / 5000
    , valueMinorStar      = 90
    , valueMajorStar      = 120
    , valueArclightEnergy = 100 / 500
    , valueRareCore       = 250
    , valueLeftoverTicket = 0
    }

scoreReward :: Valuation -> Reward -> Double
scoreReward v r =
    fromIntegral (rewardGold r)
  + fromIntegral (rewardExp r)            * valueExp v
  + fromIntegral (rewardMinorStars r)     * valueMinorStar v
  + fromIntegral (rewardMajorStars r)     * valueMajorStar v
  + fromIntegral (rewardArclightEnergy r) * valueArclightEnergy v
  + fromIntegral (rewardRareCores r)      * valueRareCore v

scoreLeftoverTickets :: Valuation -> Int -> Double
scoreLeftoverTickets v ticketsLeft =
  fromIntegral ticketsLeft * valueLeftoverTicket v

data EventName
  = DarkmoonFaire
  deriving (Eq, Show, Read, Enum, Bounded)

data InputMode
  = CliMode
  | TuiMode
  deriving (Eq, Show, Read, Enum, Bounded)

data Config = Config
  { eventName    :: !EventName
  , inputMode    :: !InputMode
  , tickets      :: !Int
  , valuation    :: !Valuation
  , debugEnabled :: !Bool
  } deriving (Eq, Show)

data NodePayload = NodePayload
  { payloadCost    :: !Int
  , payloadReward  :: !Reward
  , payloadClaimed :: !Bool
  } deriving (Eq, Show)

defaultPayload :: NodePayload
defaultPayload =
  NodePayload
    { payloadCost    = 0
    , payloadReward  = zeroReward
    , payloadClaimed = False
    }

data Node = Node
  { nodeId    :: !NodeId
  , row       :: !Row
  , cost      :: !Int
  , reward    :: !Reward
  , parents   :: ![NodeId]
  , isClaimed :: !Bool
  } deriving (Eq, Show)

type Graph = [Node]

data ValidationError
  = DuplicateNodeId NodeId
  | UnknownNodeId NodeId
  | DisconnectedClaim NodeId
  deriving (Eq, Show)

data LogPhase
  = GraphBuild
  | Validation
  | Scoring
  | Ticketing
  | Solving
  | Session
  deriving (Eq, Ord, Show)

data LogEntry = LogEntry
  { logPhase   :: !LogPhase
  , logMessage :: !String
  } deriving (Eq, Show)