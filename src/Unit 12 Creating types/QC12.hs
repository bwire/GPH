type FirstName = String
type LastName = String
type Age = Int
type Height = Int
type PatientName = (FirstName, LastName)

patientInfo :: PatientName -> Age -> Height -> String
patientInfo name age height = pName ++ " " ++ ageHeight
 where pName = snd name ++ ", " ++ fst name
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

data Sex = Male | Female deriving (Show)      
data RhType = Pos | Neg
data ABOType = A | B | AB | O
data BloodType = BloodType ABOType RhType

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"

showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"

showBloodType :: BloodType -> String
showBloodType (BloodType abo rh)  = showABO abo ++ showRh rh

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False

type MiddleName = String
data Name = Name FirstName LastName| NameWithMiddle FirstName MiddleName LastName
          
showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age :: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

johnDoe :: Patient
johnDoe = Patient (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

jeSmith :: Patient
jeSmith = Patient (NameWithMiddle "Jane" "Elizabeth" "Smith") Female 40 75 180 (BloodType O Neg)

jackieSmith :: Patient
jackieSmith = Patient {name = Name "Jackie" "Smith"
                      , age = 43
                      , sex = Female
                      , height = 62
                      , weight = 115
                      , bloodType = BloodType O Neg }
                      
canPatientDonateTo :: Patient -> Patient -> Bool
canPatientDonateTo p1 p2 = canDonateTo (bloodType p1) (bloodType p2) 

patientSummary :: Patient -> IO ()
patientSummary p = 
  putStrLn "**************" 
    >> putStrLn (showName $ name p)
    >> putStrLn ("Sex: " ++ show (sex p))
    >> putStrLn ("Age: " ++ show (age p))
    >> putStrLn ("Height: " ++ show (height p) ++ " in.")
    >> putStrLn ("Weight: " ++ show (weight p) ++ " lbs.")
    >> putStrLn ("Blod Type: " ++ showBloodType (bloodType p))