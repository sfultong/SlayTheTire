module DataTypes where
    
import Safe

data Card
  = Card
  {  cardName :: String
  ,  cost :: Int
  ,  cardHurt :: Int
  ,  cardBlock :: Int
  }
  deriving (Eq, Show)

data Player
  = Player
  {  playerHealth :: Int
  ,  playerBlock :: Int
  ,  playerMana :: Int
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
