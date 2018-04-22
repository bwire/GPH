module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Network.HTTP.Types

type HttpToken = BC.ByteString
type HttpHost = BC.ByteString
type HttpMethod = BC.ByteString
type HttpPath = BC.ByteString

myToken :: HttpToken
myToken = "yxBMnqGkjbGcefJGdiuLihPzfCdNBDuf"

noaaHost :: HttpHost
noaaHost = "www.ncdc.noaa.gov"

apiPath :: HttpPath
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: HttpToken -> HttpHost -> HttpMethod -> HttpPath -> Request
buildRequest token host method path =
  setRequestMethod method $
  setRequestHost host $
  setRequestHeader "token" [myToken] $
  setRequestPath path $
  setRequestSecure True $ 
  setRequestPort 443 defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

-- Quick Check 9.2
--There’s also a getResponseHeader function. Use both <$> and <- to get the header of the response.
qc39_2 :: IO ()
qc39_2 = httpLBS "http://news.ycombinator.com" >>= mapM_ print . getResponseHeaders 

-- Q39.1
-- Build a function buildRequestNOSSL that works exactly like buildRequest, only it doesn’t support SSL.
buildRequestNOSSL :: HttpToken -> HttpHost -> HttpMethod -> HttpPath -> Request
buildRequestNOSSL token host method path =
  setRequestMethod method $
  setRequestHost host $
  setRequestHeader "token" [myToken] $
  setRequestPath path $
  setRequestSecure False $ 
  setRequestPort 80 defaultRequest

--Q39.2
-- Improve the output of your code when something goes wrong. 
-- getResponseStatus will give you a data type including both the statusCode and the statusMessage. 
-- Fix main so that if you do get a non-200 statusCode, you print out the appropriate error.

main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatus response
  if statusCode status == 200
  then do
    print "Saving result to a file"
    L.writeFile "data.json" $ getResponseBody response
  else print $ statusMessage status


