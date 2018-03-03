import Data.List (sort)

-- Q29.1 To prove that Applicative is strictly more powerful than Functor, 
-- write a universal version of fmap, called allFmap, 
-- that defines fmap for all members of the Applicative type class. 
-- Because it works for all instances of Applicative, 
-- the only functions you can use are the methods required by the Applicative type class. 
-- To get you started, here’s your type signature:

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap f = (<*>) (pure f)


-- Q29.2 Translate the following expression into one where the result is a Maybe Int. 
-- The catch is that you may not add (or remove) anything to the code except pure and <*>. 
-- You can’t use the Just constructor or any extra parentheses. 

example :: Int
example = (*) ((+) 2 4) 6

exampleInt :: Maybe Int
exampleInt = (*) <$> (pure 6) <*> ((+) <$> (pure 2) <*> (pure 4))

--Q29.3 Take the following example and use nondeterministic computing with Lists 
-- to determine how much beer you need to purchase to assure there will be enough:

-- You bought beer last night but don’t remember whether it was a 6-pack or a 12-pack.
-- You and your roommate each had two beers last night.
-- You’re having either two or three friends coming over tonight, depending on who can come.
 -- For a long night of gaming, you expect the average person to drink three to four beers.

beerToBuy :: Int
beerToBuy =
  let be = sort $ [flip (-) 2, flip (-) 4] <*> [6, 12]
      bn = reverse . sort $ [(*3), (*4)] <*> [2, 3]
  in head bn - head be