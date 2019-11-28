module Main where

import Safe
import Data.Char (toLower)
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

showTitle :: IO()
showTitle =
  putStrLn "\n=== Slay the Tire ==="

showCards :: Player -> String
-- This doesn't work so well anymore now that cards are records
showCards = unlines . map show . zip [1..] . playerHand

initialDeck :: [Card]
initialDeck = catMaybes [Map.lookup c namedCardsMap | c <- ["Slap", "Slap", "Punch", "Guard", "Hit & Run"] ]

removeAtIndex :: Int -> [a] -> ([a], a)
removeAtIndex x = (\(hl,tl) -> (hl ++ tail tl, head tl)) . splitAt (x - 1)

removeCard :: Int -> Player -> (Player, Card)
removeCard x player = let (newCards, card) = removeAtIndex x (playerHand player)
                         in (player {playerHand = newCards}, card)

shuffle :: (GameState, [a]) -> (GameState, [a])
shuffle = id

drawCardsCount :: Int -> GameState -> GameState
drawCardsCount 0 = id
drawCardsCount count =
  modifyPlayer (\p ->
    let ((newCards, newDeck), newDiscards) = if count > length (playerDeck p)
          then (splitAt count . shuffle $ playerDeck p ++ playerDiscards p, [])
          else (splitAt count $ playerDeck p, playerDiscards p)
    in p{
      playerHand = playerHand p ++ newCards,
      playerDeck = newDeck,
      playerDiscards = newDiscards
  })

drawCards :: GameState -> GameState
drawCards gameState = 
  drawCardsCount (playerDraw $ player gameState ) gameState

getPlayedCard :: Player -> IO (Maybe(Player, Card))
getPlayedCard player = do
  putStrLn "Cards:"
  putStrLn $ showCards player
  putStrLn "Enter the number of the card to play, or p to pass:"
  playerInput <- getLine
  let
    selection :: Maybe Int
    selection = readMay playerInput
    -- IDEA: let player borrow resources from future, but has to "send to past" later or universe is destroyed
    validSelection = case selection of
      Just x | x > 0 && x <= length (playerHand player) -> Just x
      _ -> Nothing
    validMana = case validSelection of
      Just x -> cost(snd(removeCard x player)) <= playerMana player
      Nothing -> False
  case (validSelection, validMana, playerInput) of
    (Just x, True, _) -> pure $ pure $ removeCard x $ player
    (Just _, False, _) -> do
      putStrLn "Not enough mana."
      getPlayedCard player
    (Nothing, _, s) | toLower (head s) == 'p' ->
      pure Nothing
    (Nothing, _, _) -> do
      putStrLn "Invalid selection."
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
      <> ". Intent: " <> case currentIntent of
      IntentHurt x -> "hurt for " <> show x <> "\n"
      IntentBuff -> "apply a buff\n"

initialPlayer :: Player
initialPlayer = Player
  { playerHealth = 10, playerBlock = 0, playerMana = 3, playerManaMax = 3, playerDraw = 2
  , playerDeck = initialDeck, playerHand = [], playerDiscards = []
  }

playCard :: Card -> GameState -> GameState
playCard card gameState =
  modifyPlayer (\p -> p{
    playerBlock = playerBlock p + cardBlock card,
    playerMana = playerMana p - cost card,
    playerDiscards = playerDiscards p ++ [card]
    }) $
  modifyEnemy (\e -> e{
    enemyHealth=enemyHealth e - cardHurt card
  }) gameState

doIntent :: GameState -> GameState
doIntent gameState =
  let activeIntent = head (intents enemy')
      newIntents = tail (intents enemy')
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
  modifyPlayer (\p -> p{playerBlock=0, playerMana = playerManaMax p}) gameState

showBattleStatus :: GameState -> IO()
showBattleStatus g@(GameState player' enemy') = do
  showPlayerStatus player'
  showEnemyStatus enemy'

playerTurnLoop :: GameState -> IO GameState
playerTurnLoop g@(GameState player' _) = do
  showBattleStatus g
  playCardResult <- getPlayedCard player'
  case playCardResult of
    Nothing -> pure g
    Just (playedPlayer, selectedCard) -> do
      putStrLn $ "The card selected: " <> show selectedCard
      let modifyGame = playCard selectedCard . modifyPlayer (const playedPlayer)
      playerTurnLoop $ modifyGame g

enemyTurn :: GameState -> IO GameState
enemyTurn g@(GameState player' enemy') = do
  pure $ doIntent g

enemiesTurn :: GameState -> IO GameState
enemiesTurn g@(GameState player' enemy') =
  -- we'll have a list of enemies to make act later
  enemyTurn g

battleTurnLoop :: GameState -> IO()
battleTurnLoop g@(GameState player' enemy') = do
  playedCardsState <- playerTurnLoop $ drawCards g
  enemiesActedState <- enemiesTurn playedCardsState
  let cleanedState = roundCleanup enemiesActedState
    in battleTurnLoop cleanedState

main :: IO ()
main = do
  showTitle
  let tireEnemy = Map.lookup "Tire" namedEnemiesMap
  case tireEnemy of  
    Just e -> battleTurnLoop $ GameState initialPlayer e
    Nothing -> putStrLn "You Win!"
