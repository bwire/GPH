ifEven :: (Int -> Int) -> Int -> Int
ifEven f x = if even x then f x else x

inc :: Int -> Int
inc = (+1)

double :: Int -> Int
double = (*2)

square :: Int -> Int
square = (^2)
 
ifEvenInc = ifEven inc
ifEvenDouble = ifEven double
ifEvenSquare = ifEven square