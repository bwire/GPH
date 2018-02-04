-- Quick check 5.2 Write a version of genApiRequestBuilder that also takes the resource as an argument.
getRequestURL :: String -> String -> String -> String -> String
getRequestURL host apiKey res id = 
  host ++ "/" ++
  res ++ "/" ++
  id ++ "?token=" ++ apiKey

genHostRequestBuilder :: String -> (String -> String -> String -> String)
genHostRequestBuilder host = \apiKey res id -> getRequestURL host apiKey res id

genApiRequestBuilder :: (String -> String -> String -> String) -> String -> (String -> String -> String)
genApiRequestBuilder hostBuilder apiKey = \res id -> hostBuilder apiKey res id

genApiRequestBuilder' :: (String -> String -> String -> String) -> String -> String -> (String -> String)
genApiRequestBuilder' hostBuilder apiKey res = \id -> hostBuilder apiKey res id