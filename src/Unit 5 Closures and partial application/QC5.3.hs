-- Quick check 5.3 Make a builder function that’s specifically for http://example.com, 
-- the 1337hAsk3ll API key, and the book resource. 
-- That’s a function that requires only the ID of a specific book and then generates the full URL.

getRequestURL :: String -> String -> String -> String -> String
getRequestURL host apiKey res id = 
  host ++ "/" ++
  res ++ "/" ++
  id ++ "?token=" ++ apiKey
  
exampleBuilder :: String -> String -> String -> (String -> String)  
exampleBuilder = getRequestUrl "http://example.com" "1337hAsk3ll" "books"