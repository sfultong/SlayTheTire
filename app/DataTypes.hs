{-# LANGUAGE TemplateHaskell #-}
module DataTypes where

import Control.Lens.TH
import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Safe
import System.Random

data Card
  = Card
  {  _cardName :: String
  ,  _cost :: Int
  ,  _cardHurt :: Int
  ,  _cardBlock :: Int
  }
  deriving (Eq)
$(makeLenses ''Card)

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
$(makeLenses ''Player)

data Intent
  = IntentHurt
    { _intentHurt :: Int }
  | IntentBuff
  deriving (Eq, Show)
$(makeLenses ''Intent)

data Enemy
  = Enemy
  {  _enemyName :: String
  ,  _enemyHealth :: Int
  ,  _enemyBlock :: Int
  ,  _intents :: [Intent]
  }
  deriving (Eq, Show)
$(makeLenses ''Enemy)

data GameState
  = GameState
  {  _player :: Player
  ,  _enemy :: Enemy
  ,  _randomGen :: StdGen
  }
$(makeLenses ''GameState)

instance Eq GameState where 
  (==) (GameState player1 enemy1 _) (GameState player2 enemy2 _) = player1 == player2 && enemy1 == enemy2
