-- Q17.2 If your Events and Probs types were data types and not just synonyms, 
-- you could make them instances of Semigroup and Monoid, 
-- where combineEvents and combineProbs were the <> operator in each case. 
-- Refactor these types and make instances of Semigroup and Monoid.
import Data.Semigroup

newtype Events = Events { getEvents :: [String] }
newtype Probs = Probs { getProbs :: [Double] }

instance Semigroup Events where
  (<>) = combineEvents

instance Semigroup Probs where
  (<>) = combineProbs

instance Monoid Events where
  mempty = Events mempty
  mappend = (<>)

instance Monoid Probs where
  mempty = Probs mempty
  mappend = (<>)

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = 
  let
    lstProbs = getProbs probs
    normalized = map (flip (/) (sum lstProbs)) lstProbs
  in PTable events (Probs normalized)
 
showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair (getEvents events) (getProbs probs)

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f l1 l2 = 
  let nToAdd = length l2
      repeatedL1 = map (take nToAdd . repeat) l1
      newL1 = mconcat repeatedL1
      cycledL2 = cycle l2
  in zipWith f newL1 cycledL2

combineEvents :: Events -> Events -> Events
combineEvents (Events e1) (Events e2) = Events $ cartCombine (\x y -> mconcat [x, "-", y]) e1 e2

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs p1) (Probs p2) = Probs $ cartCombine (*) p1 p2

instance Semigroup PTable where
  pTable1 <> PTable (Events []) (Probs []) = pTable1
  PTable (Events []) (Probs []) <> pTable2 = pTable2
  PTable e1 p1 <> PTable e2 p2 = createPTable (combineEvents e1 e2) (combineProbs p1 p2)

instance Monoid  PTable where
  mempty = PTable mempty mempty
  mappend = (<>)