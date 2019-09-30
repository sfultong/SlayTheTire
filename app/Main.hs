module Main where

import Safe

specialLazy :: Bool
specialLazy = False && True

printThing :: IO ()
printThing = do
  putStrLn "Thing"
  putStrLn "Extra line for stuff"

myApp :: (a -> b) -> a -> b
myApp = ($)

funConcat :: (b -> c) -> (a -> b) -> (a -> c)
funConcat = (.)

-- g(f(x))
-- g . f $ x
-- for F#, we have the (|>) operator
-- doFirst |> doSecond |> doThird

addOne :: Int -> Int
addOne = (+ 1)

addOne' :: Int -> Int
addOne' x = x + 1

--addTwo :: (Int -> Int) -> Int -- ERROR!
addTwo :: Int -> (Int -> Int)
-- addTwo :: Int -> Int -> Int
addTwo = (+)

addTheNumberTwo :: Int -> Int
addTheNumberTwo = addTwo 2

addDifferentTypes :: Int -> Float -> Float
addDifferentTypes x y = fromIntegral x + y

data Thing
  = OneThing
  | TwoThing
  | ThreeThing
  deriving (Eq, Show)

convertThing :: Thing -> Thing
convertThing OneThing = TwoThing
convertThing TwoThing = ThreeThing
convertThing ThreeThing = OneThing

data PolymorphicThing a
  = PolyOne a
  | PolyTwo a a
  | PolyThree a a a
  deriving (Eq, Show)

type PolyInt = PolymorphicThing Int

firstPolyArg :: PolyInt -> Int
firstPolyArg x = case x of -- case statement means pattern matching, like the left-hand side of some functions
  PolyOne a -> a
  PolyTwo b _ -> b
  PolyThree a _ _ -> a

getHealth :: Player -> Int
getHealth = playerHealth

pairThing :: (Int, String)
pairThing = (5, "hey theer")

------------------------------ end didactic junk section ----------------------------

data Card
  = Hurt Int
  | Block Int
  deriving (Eq, Show)

data Player
  = Player
    { playerHealth :: Int
    , playerBlock :: Int
    , cards :: [Card]
  }
  deriving (Eq, Show)

data Intent
  = IntentHurt
    { intentHurt :: Int }
  | IntentBuff  
  deriving (Eq, Show)

data Enemy
  = Enemy
  {  enemyHealth :: Int
  ,  intents :: [Intent]
  }
  deriving (Eq, Show)

data GameState
  = GameState
  {  player :: Player
  ,  enemy :: Enemy
  }
  deriving (Eq, Show)

showCards :: Player -> String
showCards player = unlines . map show . zip [1..] $ cards player

exampleHand :: [Card]
exampleHand = [Hurt 2, Block 3, Hurt 7]

exampleIntents :: [Intent]
exampleIntents = [IntentHurt 4, IntentBuff, IntentHurt 2, IntentHurt 8]

removeAtIndex :: Int -> [a] -> ([a], a)
removeAtIndex x = (\(hl,tl) -> (hl ++ tail tl, head tl)) . splitAt (x - 1)

removeCard :: Int -> Player -> (Player, Card)
removeCard x player = let (newCards, card) = removeAtIndex x (cards player)
                         in (player {cards = newCards}, card)

getPlayedCard :: Player -> IO (Player, Card)
getPlayedCard player = do
  putStrLn $ showCards player
  putStrLn "Enter the number of the card to play"
  playerInput <- getLine
  let
    selection :: Maybe Int
    selection = readMay playerInput
    validSelection = case selection of
      Just x | x > 0 && x <= length (cards player) -> Just x
      _ -> Nothing
  case validSelection of
    Just x -> pure . removeCard x $ player
    Nothing -> do
      putStrLn "Invalid selection"
      getPlayedCard player

showEnemyIntent :: Enemy -> IO ()
showEnemyIntent enemy =
  let currentIntent = head (intents enemy)
  in putStrLn $ "The enemy intends to " <> case currentIntent of
    IntentHurt x -> "hurt for " <> show x
    IntentBuff -> "apply a buff"

initialPlayer :: Player
--initialPlayer = Player 10 exampleHand
initialPlayer = Player {playerHealth = 10, cards = exampleHand}

initialEnemy :: Enemy
initialEnemy = Enemy {enemyHealth = 10, intents = exampleIntents }

playCard :: Card -> Player -> Enemy -> (Player, Enemy)
playCard card player enemy =
  case card of
    Hurt x -> (player, enemy{enemyHealth=enemyHealth enemy - x})
    Block x -> (player{playerBlock=x}, enemy)

doIntent :: Enemy -> Player -> (Enemy, Player)
doIntent enemy player =
  let activeIntent = head (intents enemy)
      remaningIntents = tail (intents enemy)
  in case intent of
    IntentHurt x -> (enemy{intents=remaningIntents}, player{playerHealth=playerHealth player - x})
    IntentBuff -> (enemy{intents=remaningIntents}, player)

gameLoop :: (Player, Enemy) -> IO()
gameLoop (player, enemy) = do
  showEnemyIntent enemy
  (newCombatant, selectedCard) <- getPlayedCard player
  putStrLn $ "The card selected: " <> show selectedCard
  let (player, enemy) = playCard selectedCard player enemy
      (player, enemy) = doIntent enemy player
  gameLoop (player, enemy)



main :: IO ()
main = do
  --putStrLn "Example card hand:"
  -- putStrLn . showCards $ initialPlayer
  gameLoop (initialPlayer, initialEnemy)
