module Enemies where

import Control.Lens hiding (element)
import DataTypes
import Data.Map (Map)
import Safe
import qualified Data.Map as Map


allEnemies = [
    Enemy {
    _enemyName = "Tire",
    _enemyHealth = 18,
    _enemyBlock = 0,
    _intents = cycle[IntentHurt 1, IntentBuff, IntentHurt 3]
},
    Enemy {
    _enemyName = "Wollypobber",
    _enemyHealth = 10,
    _enemyBlock = 0,
    _intents = cycle[IntentHurt 4, IntentBuff, IntentHurt 2, IntentHurt 8]
}]

namedEnemiesList = [(view enemyName e, e) | e <- allEnemies ]

namedEnemiesMap = Map.fromList namedEnemiesList
