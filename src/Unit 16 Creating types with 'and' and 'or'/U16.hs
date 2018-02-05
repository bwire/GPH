type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName    
          | NameWithMiddle FirstName MiddleName LastName    
          | TwoInitialsWithLast Char Char LastName
          | FirstNameWithTwoInits FirstName Char Char deriving (Show)

data Author = Author Name deriving (Show)
data Artist = Person Name | Band String deriving (Show)
data Creator = AuthorCreator Author | ArtistCreator Artist deriving (Show)

data Book = Book 
  { author :: Creator    
  , isbn :: String    
  , bookTitle :: String    
  , bookYear  :: Int    
  , bookPrice :: Double }

data VinylRecord = VinylRecord 
  { artist :: Creator    
  , recordTitle :: String    
  , recordYear :: Int    
  , recordPrice   :: Double }

data CollectibleToy = CollectibleToy 
  { name :: String    
  , descrption :: String    
  , toyPrice :: Double    
  }

-- Q16.1 Q16.1 To further complicate the items in your store, 
-- you eventually keep an inventory of free pamphlets. 
-- Pamphlets have a title, a description, and a contact field for the organization that provides the pamphlet. 
-- Create the Pamphlet type and add it to StoreItem. Additionally, modify the price so that it works with Pamphlet.
data Pamflet = Pamflet
  { pamfletTitle :: String
  , pamfletDescription :: String
  , contacts :: String
  , pamfletPrice :: Double
}


data StoreItem = BookItem Book 
               | RecordItem VinylRecord
               | ToyItem CollectibleToy
               | PamfletItem Pamflet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamfletItem pamflet) = pamfletPrice pamflet

madeBy :: StoreItem -> String
madeBy (BookItem book) = show . author $ book
madeBy (RecordItem record) = show . artist $ record
madeBy _ = "unknown"