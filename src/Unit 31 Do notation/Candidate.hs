module Candidate where

import Data.Map as Map

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)
data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
   { candidateId :: Int
   , codeReview :: Grade
   , cultureFit :: Grade
   , education :: Degree } deriving Show


viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where passedCoding = codeReview candidate > B
        passedCultural = cultureFit candidate > C
        educationMin = education candidate >= MS
        tests = [passedCoding, passedCultural, educationMin]

readInt :: IO Int
readInt = getLine >>= return . read

readGrade :: IO Grade
readGrade = getLine >>= return . read

readDegree :: IO Degree
readDegree = getLine >>= (return . read)

readCandidate :: IO Candidate
readCandidate = do
   putStrLn "enter id:"
   cId <- readInt
   putStrLn "enter code grade:"
   codeGrade <- readGrade
   putStrLn "enter culture fit grade:"
   cultureGrade <- readGrade
   putStrLn "enter education:"
   degree <- readDegree
   return $ Candidate cId codeGrade cultureGrade degree

accessCandidateIO :: IO String
accessCandidateIO = do
  candidate <- readCandidate
  if viable candidate
  then return "passed"
  else return "failed" 

candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                       , codeReview = A
                       , cultureFit = A
                       , education = BA }
 
candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                       , codeReview = C
                       , cultureFit = A
                       , education = PhD }
 
 
candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                       , codeReview = A
                       , cultureFit = B
                       , education = MS }

candidatesDB :: Map.Map Int Candidate
candidatesDB = Map.fromList [(1, candidate1), (2, candidate2), (3, candidate3)]

accessCandidateMaybe :: Int -> Maybe String
accessCandidateMaybe id = do
  candidate <- Map.lookup id candidatesDB
  if viable candidate
  then return "passed"
  else return "failed"

candidates :: [Candidate]
candidates = [candidate1
             ,candidate2
             ,candidate3]

accessCandidateList :: [Candidate] -> [String]
accessCandidateList list = do
  candidate <- list
  if viable candidate
  then return "passed"
  else return "failed"

accessCandidate :: Monad m => m Candidate -> m String
accessCandidate wrapped = do
  candidate <- wrapped
  if viable candidate
  then return "passed"
  else return "failed"  