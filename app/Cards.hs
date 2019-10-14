module Cards where

import Safe
import DataTypes
import Data.Map (Map)
import qualified Data.Map as Map

allCards = [
    Card {
    cardName = "Slap",
    cost = 1,
    cardHurt = 3,
    cardBlock = 0
  },
    Card {
    cardName = "Punch",
    cost = 2,
    cardHurt = 7,
    cardBlock = 0
  },
  Card {
    cardName = "Hit & Run",
    cost = 3,
    cardHurt = 4,
    cardBlock = 5
  },
  Card {
    cardName = "Guard",
    cost = 1,
    cardHurt = 0,
    cardBlock = 5
    },
  Card {
    cardName = "Slay",
    cost = 4,
    cardHurt = 15,
    cardBlock = 0
}]

namedCardsList = [(cardName e, e) | e <- allCards ]

namedCardsMap = Map.fromList namedCardsList
