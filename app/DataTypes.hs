module DataTypes where

import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Safe
import System.Random

data Error
  = Error{
    errorMessage :: String
  }
  deriving(Eq, Show)

data TargetType
  = Untargeted
  | Targeted
  deriving(Eq)

data CardDetail
  = ItHurts {
  }

data Card
  = Card
  {  _cardName :: String
  ,  _cost :: Int
  ,  _targetType :: TargetType
  ,  _modifyWorld :: Card -> GameState -> (String, GameState)
  ,  _cardDescription :: String
  }
--  deriving (Eq)

instance Eq Card where
  (==) (Card cn c tt _ cd) (Card cn' c' tt' _ cd') = cn == cn' && c == c' && tt == tt' && cd == cd'

instance Show Card where
  show (Card cname ccost _ _ desc) = cname <> " (" <> show ccost <> ")\n" <> show desc

data Player
  = Player
  {  _playerHealth :: Int
  ,  _playerBlock :: Int
  ,  _playerMana :: Int
  ,  _playerManaMax :: Int
  ,  _playerDraw :: Int
  ,  _playerDeck :: [Card]
  ,  _playerHand :: [Card]
  ,  _playerActiveDiscards :: [Card]
  ,  _playerDiscards :: [Card]
  }
  deriving (Eq, Show)

data Intent
  = IntentHurt
    { _intentHurt :: Int }
  | IntentBuff
  deriving (Eq, Show)

data Enemy
  = Enemy
  {  _enemyName :: String
  ,  _enemyHealth :: Int
  ,  _enemyBlock :: Int
  ,  _intents :: [Intent]
  }
  deriving (Eq, Show)

data Location
  = Battle
  {  _enemies :: [Enemy]
  }
  | Campfire
  deriving (Eq, Show)

data GameState
  = GameState
  {  _player :: Player
  ,  _location :: Location
  ,  _randomGen :: StdGen
  }

instance Eq GameState where
  (==) (GameState player1 enemy1 _) (GameState player2 enemy2 _) = player1 == player2 && enemy1 == enemy2
