module Enemies where
    
import Safe
import DataTypes
import Data.Map (Map)
import qualified Data.Map as Map


allEnemies = [
    Enemy {
    enemyName = "Tire",
    enemyHealth = 18,
    enemyBlock = 0,
    intents = [IntentHurt 1, IntentBuff, IntentHurt 3]
},  
    Enemy {
    enemyName = "Wollypobber",
    enemyHealth = 10,
    enemyBlock = 0,
    intents = [IntentHurt 4, IntentBuff, IntentHurt 2, IntentHurt 8]
}]

namedEnemiesList = [(enemyName e, e) | e <- allEnemies ]

namedEnemiesMap = Map.fromList namedEnemiesList
