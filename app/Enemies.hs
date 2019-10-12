module Enemies where
    
import Safe
import DataTypes
import Data.Map (Map)
import qualified Data.Map as Map


wollypobber = Enemy {
        enemyName = "Wollypobber",
        enemyHealth = 10,
        enemyBlock = 0,
        intents = [IntentHurt 4, IntentBuff, IntentHurt 2, IntentHurt 8]
    }

-- allEnemiesList :: [(String, Enemy)]
allEnemiesList = [(enemyName wollypobber, wollypobber)]

-- allEnemiesMap :: Map String Enemy
allEnemiesMap = Map.fromList allEnemiesList
