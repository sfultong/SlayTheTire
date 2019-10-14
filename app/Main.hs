module Main where

import Safe
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import Enemies
import Cards
import DataTypes

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
modifyPlayer :: (Player -> Player) -> GameState -> GameState
modifyPlayer f gameState = gameState {player = f $ player gameState}
modifyEnemy :: (Enemy -> Enemy) -> GameState -> GameState
modifyEnemy f gameState = gameState {enemy = f $ enemy gameState}

showCards :: Player -> String
showCards player = unlines . map show . zip [1..] $ cardHand player

exampleHand :: [Card]
exampleHand = catMaybes [Map.lookup c namedCardsMap | c <- ["Slap", "Slap", "Punch", "Guard", "Hit & Run"] ]

removeAtIndex :: Int -> [a] -> ([a], a)
removeAtIndex x = (\(hl,tl) -> (hl ++ tail tl, head tl)) . splitAt (x - 1)

removeCard :: Int -> Player -> (Player, Card)
removeCard x player = let (newCards, card) = removeAtIndex x (cardHand player)
                         in (player {cardHand = newCards}, card)

getPlayedCard :: Player -> IO (Player, Card)
getPlayedCard player = do
  putStrLn "Cards:"
  putStrLn $ showCards player
  putStrLn "Enter the number of the card to play, or p to pass:"
  playerInput <- getLine
  let
    selection :: Maybe Int
    selection = readMay playerInput
    validSelection = case selection of
      Just x | x > 0 && x <= length (cardHand player) -> Just x
      _ -> Nothing
  case validSelection of
    Just x -> pure . removeCard x $ player
    Nothing -> do
      putStrLn "Invalid selection"
      getPlayedCard player

showPlayerStatus :: Player -> IO ()
showPlayerStatus player =
  putStrLn $ "Player: Health " <> show(playerHealth player) <>
    ", Block " <> show(playerBlock player) <>
    ", Mana: " <> show(playerMana player)

showEnemyStatus :: Enemy -> IO ()
showEnemyStatus enemy =
  let currentIntent = head (intents enemy) in
    putStrLn $ enemyName enemy <> ": Health " <> show(enemyHealth enemy) <> ", Block " <> show(enemyBlock enemy)
      <> "  Intent: " <> case currentIntent of
      IntentHurt x -> "hurt for " <> show x <> "\n"
      IntentBuff -> "apply a buff\n"

initialPlayer :: Player
initialPlayer = Player
  { playerHealth = 10, playerBlock = 0, playerMana = 3, playerDraw = 2
  , cardDeck = [], cardHand = exampleHand, cardDiscards = []
  }

playCard :: Card -> GameState -> GameState
playCard card gameState =
  modifyPlayer (\p -> p{
    playerBlock = playerBlock p + cardBlock card,
    playerMana = playerMana p - cost card
    }) $
  modifyEnemy (\e -> e{
    enemyHealth=enemyHealth e - cardHurt card
  }) gameState

doIntent :: GameState -> GameState
doIntent gameState =
  let activeIntent = head (intents enemy')
      newIntents = tail (intents enemy') ++ [activeIntent]
      enemy' = enemy gameState
  in case activeIntent of
    IntentHurt h -> modifyEnemy (\e -> e{
      intents=newIntents
      }) $
      modifyPlayer (\p -> p{
        playerHealth=minimum [playerHealth p, playerHealth p + playerBlock p - h]
      }) gameState
    IntentBuff -> modifyEnemy (\e -> e{intents=newIntents}) gameState

roundCleanup :: GameState -> GameState
roundCleanup gameState =
  modifyPlayer (\p -> p{playerBlock=0}) gameState

playerTurnLoop :: GameState -> IO()
playerTurnLoop g@(GameState player' enemy') = do
  showPlayerStatus player'
  showEnemyStatus enemy'
  (newCombatant, selectedCard) <- getPlayedCard player'
  putStrLn $ "The card selected: " <> show selectedCard
  let modifyGame = playCard selectedCard . modifyPlayer (const newCombatant)
  playerTurnLoop $ modifyGame g

enemiesTurn :: GameState -> GameState
enemiesTurn g@(GameState player' enemy') = do
  doIntent g

main :: IO ()
main = do
  let firstEnemy = Map.lookup "Wollypobber" namedEnemiesMap
  case firstEnemy of  
    Just e -> playerTurnLoop $ GameState initialPlayer e
    Nothing -> putStrLn "You Win!"
