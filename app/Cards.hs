module Cards where

import Control.Lens hiding (element)
import DataTypes
import Lenses
import Data.Map (Map)
import Safe
import qualified Data.Map as Map


-- firstEnemy :: Lens' Location Enemy
firstEnemy :: Functor f => (Enemy -> f Enemy) -> Location -> f Location
firstEnemy f (Battle (frenemy:rest)) = fmap (\fremeny' -> Battle (fremeny':rest)) (f frenemy)
firstEnemy _ _ = error "firstEnemy lens needs further work"

allCards = [
    Card {
    _cardName = "Slap",
    _cost = 1,
    {-
    _cardHurt = 3,
    _cardBlock = 0
-}
    _cardDescription = "Slap with an open hand",
    _modifyWorld = \_ gs -> ("you slapped a thing", over (location . firstEnemy . enemyHealth) (subtract 3) gs)
  },
    Card {
    _cardName = "Punch",
    _cost = 2,
    {-
    _cardHurt = 7,
    _cardBlock = 0
-}
    _cardDescription = "Punch",
    _modifyWorld = \this gs -> ("TODO", gs)
  },
  Card {
    _cardName = "Hit & Run",
    _cost = 3,
    {-
    _cardHurt = 4,
    _cardBlock = 5
-}
    _cardDescription = "A cowardly attack",
    _modifyWorld = \this gs -> ("TODO", gs)
  },
  Card {
    _cardName = "Guard",
    _cost = 1,
    {-
    _cardHurt = 0,
    _cardBlock = 5
-}
    _cardDescription = "protect youself",
    _modifyWorld = \this gs -> ("TODO", gs)
    },
  Card {
    _cardName = "Slay",
    _cost = 4,
    {-
    _cardHurt = 15,
    _cardBlock = 0
-}
    _cardDescription = "big hurt",
    _modifyWorld = \this gs -> ("TODO", gs)
}]

namedCardsList = [(view cardName e, e) | e <- allCards ]

namedCardsMap = Map.fromList namedCardsList
