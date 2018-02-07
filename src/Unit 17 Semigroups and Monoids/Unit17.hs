import Data.Semigroup

type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events $ map (flip (/) (sum probs)) probs
        
showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

instance Show PTable where
  show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine f l1 l2 = 
  let nToAdd = length l2
      repeatedL1 = map (take nToAdd . repeat) l1
      newL1 = mconcat repeatedL1
      cycledL2 = cycle l2
  in zipWith f newL1 cycledL2

combineEvents :: Events -> Events -> Events
combineEvents = cartCombine (\x y -> mconcat [x, "-", y]) 

combineProbs :: Probs -> Probs -> Probs
combineProbs = cartCombine (*)

instance Semigroup PTable where
  pTable1 <> PTable [] [] = pTable1
  PTable [] [] <> pTable2 = pTable2
  PTable e1 p1 <> PTable e2 p2 = createPTable (combineEvents e1 e2) (combineProbs p1 p2)

instance Monoid  PTable where
  mempty = PTable [] []
  mappend = (<>)