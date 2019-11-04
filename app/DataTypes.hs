module DataTypes where

import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Safe

data Card
  = Card
  {  cardName :: String
  ,  cost :: Int
  ,  cardHurt :: Int
  ,  cardBlock :: Int
  }
  deriving (Eq)

instance Show Card where
  show (Card cname ccost churt cblock) = cname <> " (" <> show ccost <> ")"
    <> concat (intersperse ", " $ catMaybes [hurttext, blocktext])
    where
      hurttext = if churt == 0
        then Nothing
        else pure $ " causes " <> show churt <> " damage"
      blocktext = if cblock == 0
        then Nothing
        else pure $ " protects against " <> show cblock <> " damage"

data Player
  = Player
  {  playerHealth :: Int
  ,  playerBlock :: Int
  ,  playerMana :: Int
  ,  playerManaMax :: Int
  ,  playerDraw :: Int
  ,  playerDeck :: [Card]
  ,  playerHand :: [Card]
  ,  playerDiscards :: [Card]
  }
  deriving (Eq, Show)

data Intent
  = IntentHurt
    { intentHurt :: Int }
  | IntentBuff
  deriving (Eq, Show)

data Enemy
  = Enemy
  {  enemyName :: String
  ,  enemyHealth :: Int
  ,  enemyBlock :: Int
  ,  intents :: [Intent]
  }
  deriving (Eq, Show)

data GameState
  = GameState
  {  player :: Player
  ,  enemy :: Enemy
  }
  deriving (Eq, Show)
