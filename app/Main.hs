{-# LANGUAGE TemplateHaskell #-}

module Main where

import Safe
import Control.Lens hiding (element)
import Data.Char (toLower)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import Enemies
import Cards
import DataTypes
import System.Random


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
getHealth = view playerHealth

pairThing :: (Int, String)
pairThing = (5, "hey theer")

------------------------------ end didactic junk section ----------------------------

showTitle :: IO()
showTitle =
  putStrLn "\n=== Slay the Tire ==="

showCards :: Player -> String
-- This doesn't work so well anymore now that cards are records
showCards = unlines . map show . zip [1..] . view playerHand

initialDeck :: [Card]
initialDeck = catMaybes [Map.lookup c namedCardsMap | c <- ["Slap", "Slap", "Punch", "Guard", "Hit & Run"] ]

removeAtIndex :: Int -> [a] -> ([a], a)
removeAtIndex x = (\(hl,tl) -> (hl ++ tail tl, head tl)) . splitAt (x - 1)

modifyAtIndex :: Int -> (a -> a) -> [a] -> [a]
modifyAtIndex x f = (\(hl,tl) -> (hl ++ [f (head tl)] ++ tail tl)) . splitAt (x - 1)

removeCard :: Int -> Player -> (Player, Card)
removeCard x player = let (newCards, card) = removeAtIndex x $ view playerHand player
                         in (set playerHand newCards player, card)

shuffle :: (GameState, [a]) -> (GameState, [a])
shuffle (gameState, [x]) = (gameState, [x])
shuffle (gameState, (xs)) =
  let (randomIndex, newRandomGen) = randomR (0, length xs - 1) $ view randomGen gameState
      (newCards, removedCard) = removeAtIndex randomIndex xs
      prependCard (gameState, cardList) = (gameState, removedCard : cardList)
  in prependCard $ shuffle (set randomGen newRandomGen gameState, newCards)

drawCardsCount :: Int -> GameState -> GameState
drawCardsCount 0 gameState = gameState
drawCardsCount count gameState =
  if count > length (view (player . playerDeck) gameState)
    then let p = view player gameState
             (newGameState, shuffledDeck) = shuffle (gameState, view playerDeck p ++ view playerDiscards p)
             (newCards, newDeck) = splitAt count shuffledDeck
             in over player
                ( over playerHand (++ newCards)
                . set playerDeck newDeck
                . set playerDiscards []
                ) newGameState
    else let p = view player gameState
             (newCards, newDeck) = splitAt count $ view playerDeck p
             in over player
                ( over playerHand (++ newCards)
                . set playerDeck newDeck
                ) gameState

drawCards :: GameState -> GameState
drawCards gameState = 
  drawCardsCount (view (player . playerDraw) gameState) gameState

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
      Just x | x > 0 && x <= length (view playerHand player) -> Just x
      _ -> Nothing
    validMana = case validSelection of
      Just x -> view cost(snd(removeCard x player)) <= view playerMana player
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
  putStrLn $ "Player: Health " <> show(view playerHealth player) <>
    ", Block " <> show(view playerBlock player) <>
    ", Mana: " <> show(view playerMana player)

showEnemyStatus :: Enemy -> IO ()
showEnemyStatus enemy =
  let currentIntent = head (view intents enemy) in
    putStrLn $ view enemyName enemy <> ": Health " <> show(view enemyHealth enemy) <> ", Block " <> show(view enemyBlock enemy)
      <> ". Intent: " <> case currentIntent of
      IntentHurt x -> "hurt for " <> show x <> "\n"
      IntentBuff -> "apply a buff\n"

initialPlayer :: Player
initialPlayer = Player
  { _playerHealth = 10, _playerBlock = 0, _playerMana = 3, _playerManaMax = 3, _playerDraw = 3
  , _playerDeck = initialDeck, _playerHand = [], _playerActiveDiscards = [], _playerDiscards = []
  }

playCard :: Card -> Maybe Int -> GameState -> Either Error GameState
playCard card target =
  case (view targetType card, target) of
    (Targeted, Just target) ->
      undefined
    (Untargeted, Nothing) ->
      undefined
    _ ->
      Left $ Error "Must provide appropriate target type for card."

  over player
  ( over playerBlock (+ view cardBlock card)
  . over playerMana (subtract (view cost card))
  . over playerDiscards (card :)
  )
  . over (enemies . location)
  ( over enemyHealth (subtract (view cardHurt card)))

doIntent :: GameState -> GameState
doIntent gameState =
  let activeIntent = head (view intents enemy')
      newIntents = tail (view intents enemy')
      enemy' = view enemy gameState
  in case activeIntent of
    IntentHurt h ->
      over enemy
      (set intents newIntents)
      $ over player
      ( \p -> over playerHealth (\l -> minimum [l, l + view playerBlock p - h]) p
      ) gameState
    IntentBuff -> over enemy (set intents newIntents) gameState
 
modifyEnemy :: Int -> (Enemy -> Enemy) -> GameState -> GameState
modifyEnemy idx f gameState =
  over (enemies . location) (modifyAtIndex idx f)

roundCleanup :: GameState -> GameState
roundCleanup =
  over player (\p -> set playerBlock 0 $ set playerMana (view playerManaMax p) p)

showBattleStatus :: GameState -> IO()
showBattleStatus g@(GameState player' enemy' _) = do
  showPlayerStatus player'
  showEnemyStatus enemy'

playerTurnLoop :: GameState -> IO GameState
playerTurnLoop g@(GameState player' _ _) = do
  showBattleStatus g
  playCardResult <- getPlayedCard player'
  case playCardResult of
    Nothing -> pure g
    Just (playedPlayer, selectedCard) -> do
      putStrLn $ "The card selected: " <> show selectedCard
      let modifyGame = playCard selectedCard . set player playedPlayer
      playerTurnLoop $ modifyGame g

enemyTurn :: GameState -> IO GameState
enemyTurn g = do
  pure $ doIntent g

enemiesTurn :: GameState -> IO GameState
enemiesTurn g =
  -- we'll have a list of enemies to make act later
  enemyTurn g

battleTurnLoop :: GameState -> IO()
battleTurnLoop g = do
  playedCardsState <- playerTurnLoop $ drawCards g
  enemiesActedState <- enemiesTurn playedCardsState
  let cleanedState = roundCleanup enemiesActedState
    in battleTurnLoop cleanedState

main :: IO ()
main = do
  randomGen <- getStdGen
  showTitle
  let tireEnemy = Map.lookup "Tire" namedEnemiesMap
  case tireEnemy of  
    Just e -> battleTurnLoop $ GameState initialPlayer e randomGen
    Nothing -> putStrLn "You Win!"
