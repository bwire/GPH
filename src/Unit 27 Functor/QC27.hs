import RobotPart

-- Quick check 27.1 Write the function reverseMaybe :: Maybe String -> Maybe String 
-- that reverses a Maybe String and returns it as a Maybe String.

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just s) = Just . reverse $ s

-- Quick check 27.2 Use fmap or <$> to reverse a Maybe String.
maybeReverse :: Maybe String -> Maybe String
maybeReverse = fmap reverse

-- Quick check 27.3 Rewrite the definition of all parts to use <$> instead of map.