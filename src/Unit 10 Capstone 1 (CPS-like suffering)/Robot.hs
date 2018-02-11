{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Robot where

type RTuple = (String, Int, Int)
type Robot a = forall a. (RTuple -> a) -> a

robot :: RTuple -> Robot a
robot (name, attack, hp) = \message -> message (name, attack, hp)

-- accessors
name :: RTuple -> String
name (nm, _, _) = nm

attack :: RTuple -> Int
attack (_, a, _) = a

hp :: RTuple -> Int
hp (_, _, p) = p

getName :: ((RTuple -> String) -> String) -> String
getName r = r name

getAttack :: ((RTuple -> Int) -> Int)  -> Int
getAttack r = r attack

getHP :: ((RTuple -> Int) -> Int)  -> Int
getHP r = r hp

-- setters
setName :: Robot String -> String -> Robot a
setName r nm = r $ \(_, a, hp) -> robot (nm, a, hp)

setAttack :: Robot Int -> Int -> Robot a
setAttack r a = r $ \(nm, _, hp) -> robot (nm, a, hp)

setHP :: Robot Int -> Int -> Robot a
setHP r hp = r $ \(nm, a, _) -> robot (nm, a, hp)

-- additional
printRobot :: Robot a -> String
printRobot r = r $ \(nm, a, hp) -> nm ++ " attack:" ++ show a ++ " hp:" ++ show hp

damage :: Robot a -> Int -> Robot a
damage r ad = r $ \(nm, a, hp) -> robot (nm, a, hp - ad)

fight :: Robot a -> Robot a -> Robot a
fight atacker defender = damage defender power where
  power = if getHP atacker > 10
          then getAttack atacker
          else 0

-- Write a threeRoundFight function that takes two robots and has them fight for three rounds, 
-- returning the winner. To avoid having so many different variables for robot state, 
-- use a series of nested lambda functions so you can just overwrite robotA and robotB.      
threeRoundFight :: Robot a -> Robot a -> Robot a
threeRoundFight = t' 6
  where
    t' :: Int -> Robot a -> Robot a -> Robot a
    t' 0 r1 r2 = if getHP r1 > getHP r2 then r1 else r2
    t' n r1 r2 = t' (n - 1) (fight r1 r2) r1

-- Create a list of three robots. Then create a fourth robot. 
-- Use partial application to create a closure for the fight method so the fourth robot can fight all three robots at once, 
-- using map. Finally, use map to get the remaining life from the rest of the robots.

_3robots :: [Robot Int]
_3robots = [robot ("Murderer", 20, 100), robot ("Slayer", 10, 150), robot ("Killer", 25, 120)]

-- Use map on a list of robot objects to get the life of each robot in the list
lives :: [(RTuple -> Int) -> Int] -> [Int]
lives = map getHP 

robotOfLight :: Robot Int
robotOfLight = robot ("Saviour", 30, 80)

killEmAll :: Robot Int -> (Robot Int -> Robot Int)
killEmAll attacker = fight attacker

remainingLives :: [Int]
remainingLives = map (getHP . killEmAll robotOfLight) _3robots
