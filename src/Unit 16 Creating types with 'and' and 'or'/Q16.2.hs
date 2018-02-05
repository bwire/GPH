-- Q16.2 Create a Shape type that includes the following shapes: Circle, Square, and Rectangle. 
-- Then write a function to compute the perimeter of a Shape as well as its area.

data Point = Point { x :: Double, y :: Double }

data Circle = Circle { circlePosition :: Point, radius :: Double }
data Square = Square { squarePosition :: Point, side :: Double }
data Rectangle = Rectangle { rectanglePosition :: Point, height :: Double, width :: Double }

data Shape = ShapeCircle Circle | ShapeSquare Square | ShapeRectangle Rectangle

perimeter :: Shape -> Double
perimeter (ShapeCircle (Circle _ r)) = 2 * pi * r
perimeter (ShapeSquare (Square _ s)) = 4 * s
perimeter (ShapeRectangle (Rectangle _ h w)) = 2 * (h + w)

area :: Shape -> Double
area (ShapeCircle (Circle _ r)) = 4 * pi * r ^ 2
area (ShapeSquare (Square _ s)) = s * s
area (ShapeRectangle (Rectangle _ h w)) = h * w

