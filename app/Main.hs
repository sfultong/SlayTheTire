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

getHealth :: Combatant -> Int
getHealth = health

pairThing :: (Int, String)
pairThing = (5, "hey theer")

------------------------------ end didactic junk section ----------------------------

data Card
  = Hurt Int
  deriving (Eq, Show)

data Combatant
  = Combatant
    { health :: Int
    , cards :: [Card]
    }
  deriving (Eq, Show)

showCards :: Combatant -> String
showCards combatant = unlines . map show . zip [1..] $ cards combatant

exampleHand :: [Card]
exampleHand = [Hurt 2, Hurt 3, Hurt 7]

removeAtIndex :: Int -> [a] -> ([a], a)
removeAtIndex x = (\(hl,tl) -> (hl ++ tail tl, head tl)) . splitAt (x - 1)

removeCard :: Int -> Combatant -> (Combatant, Card)
removeCard x combatant = let (newCards, card) = removeAtIndex x (cards combatant)
                         in (combatant {cards = newCards}, card)

getPlayedCard :: Combatant -> IO (Combatant, Card)
getPlayedCard combatant = do
  putStrLn $ showCards combatant
  putStrLn "Enter the number of the card to play"
  playerInput <- getLine
  let
    selection :: Maybe Int
    selection = readMay playerInput
    validSelection = case selection of
      Just x | x > 0 && x <= length (cards combatant) -> Just x
      _ -> Nothing
  case validSelection of
    Just x -> pure . removeCard x $ combatant
    Nothing -> do
      putStrLn "Invalid selection"
      getPlayedCard combatant

initialPlayer :: Combatant
--initialPlayer = Combatant 10 exampleHand
initialPlayer = Combatant {health = 10, cards = exampleHand}

main :: IO ()
main = do
  --putStrLn "Example card hand:"
  -- putStrLn . showCards $ initialPlayer
  (newCombatant, selectedCard) <- getPlayedCard initialPlayer
  putStrLn $ "The card selected: " <> show selectedCard

