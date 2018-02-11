type Cup = (Int -> Int) -> Int

cup :: Int -> Cup
cup = flip ($)

getOz :: Cup -> Int 
getOz = flip ($) id 

drink :: Cup -> Int -> Cup
drink cp ozDrunk | ozDrunk > ozCont = cup 0
                 | otherwise = cup ozDiff where
                    ozCont = getOz cp
                    ozDiff = ozCont - ozDrunk
                    
isEmpty :: Cup -> Bool
isEmpty = (==0) . getOz
