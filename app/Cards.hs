module Cards where

import Control.Lens hiding (element)
import DataTypes
import Data.Map (Map)
import Safe
import qualified Data.Map as Map

allCards = [
    Card {
    _cardName = "Slap",
    _cost = 1,
    _cardHurt = 3,
    _cardBlock = 0
  },
    Card {
    _cardName = "Punch",
    _cost = 2,
    _cardHurt = 7,
    _cardBlock = 0
  },
  Card {
    _cardName = "Hit & Run",
    _cost = 3,
    _cardHurt = 4,
    _cardBlock = 5
  },
  Card {
    _cardName = "Guard",
    _cost = 1,
    _cardHurt = 0,
    _cardBlock = 5
    },
  Card {
    _cardName = "Slay",
    _cost = 4,
    _cardHurt = 15,
    _cardBlock = 0
}]

namedCardsList = [(view cardName e, e) | e <- allCards ]

namedCardsMap = Map.fromList namedCardsList
