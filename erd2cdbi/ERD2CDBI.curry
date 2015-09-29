--- ----------------------------------------------------------------------------
--- This module creates all the needed datatypes, a SQLite database from an
--- x_ERDT.term (An ER-Model that was translated by erd2curry) and a .info file
--- needed by currypp when preprocessing SQL
---
--- @author Mike Tallarek, extensions by Julia Krone
--- @version 0.2
--- ----------------------------------------------------------------------------

import Char ( toLower, toUpper )
import IO
import IOExts ( connectToCommand )
import List
import ReadShowTerm ( readsQTerm )
import System
import Time

--import Database.CDBI.Connection (SQLValue(..))

-- The ERDDatatype that everything is based on
data ERD = ERD String [Entity] [Relationship]
data Entity = Entity String [Attribute]
data Attribute = Attribute String Domain Key Null
data Key = NoKey | PKey | Unique
type Null = Bool
data Domain = IntDom (Maybe Int)
            | FloatDom (Maybe Float)
            | CharDom (Maybe Char)
            | StringDom (Maybe String)
            | BoolDom (Maybe Bool)
            | DateDom (Maybe ClockTime)
            | KeyDom String
            | UserDefined String (Maybe String)

data Relationship = Relationship String [REnd]

data REnd = REnd String String Cardinality

data Cardinality = Exactly Int
                 | Between Int MaxValue

data MaxValue = Max Int | Infinite

-- Takes a x_ERDT.term (An ER-Model that was translated by erd2curry),
-- creates a SQLite database, a .curry program with all datatypes
-- and an info file for the CurryPP-SQLParser
-- parameter: .term-file pathForModel dbPath option(-db)
main ::  IO ()
main  = 
  do args <- getArgs
     case args of
      (str  : dbPath : option) -> do
          handle <- openFile str ReadMode
          hGetLine handle
          hGetLine handle
          contents <- (hGetContents handle)     
          let newDB = case option of
                             ("-db":_)     -> True
                             _             -> False
          case (readsQTerm contents) of
              []         -> putStr "Should not happen"
              ((a,_):_)  -> writeCDBI a dbPath newDB
      _ -> showUsageString
                           
showUsageString :: IO()
showUsageString = do
  putStrLn ("usage:\n<name of term-file>\n"++
                    "<absolute path to database (including name of database)>\n"++                   
                    "optional '-db' which means that a new, empty database is generated\n")
  exitWith 1

-- Write all the data so CDBI can be used, create a database 
-- when option is set and an info file
writeCDBI :: ERD -> String -> Bool -> IO ()
writeCDBI (ERD name ents rels)  dbPath newDB = do
  file <- openFile (name++"_CDBI"++".curry") WriteMode
  hPutStrLn file ("module "++name++"_CDBI where\n" ++               
                  "import Time (ClockTime)\n" ++
                  "import Database.CDBI.ER\n" ++
		  "import Database.CDBI.Criteria(idVal)\n"++
                  "import Database.CDBI.Connection \n"++
                  "import Database.CDBI.Description \n \n")
  mapIO_ (\x -> (writeEntityData x file)) ents
  hClose file
  file2 <- openFile (name++"_SQLParserInfo.info") WriteMode
  writeParserFile file2 ents rels dbPath
  hClose file2
  if newDB then do 
                 db <- connectToCommand $ "sqlite3 " ++ dbPath
                 createDatabase ents db
                 hClose db
           else return ()
  

-- -------------- writing file2 containing auxiliary data for parsing embedded sql expressions -------------
writeParserFile :: Handle -> [Entity] -> [Relationship] -> String -> IO()
writeParserFile file2 ents rels dbPath = do
  hPutStrLn file2 ("PInfo"++" \""++dbPath++"\"")
  hPutStrLn file2 ((spaceN 6)++"["++
                   (intercalate (",\n"++(spaceN 7)) 
                                (map (getrelationtypes ents) rels))++"]")
  hPutStrLn file2 ((spaceN 6)++"["++
                   (intercalate (",\n"++(spaceN 7))
                                (map getNullableAttr ents))++"]")
  hPutStrLn file2 ((spaceN 6)++"["++
                   (intercalate (",\n"++(spaceN 7))
                                (map getAttrList ents))++"]")
  hPutStrLn file2 ((spaceN 6)++"["++
                    (intercalate (",\n"++(spaceN 7))
                                 (map getAttrTypes ents))++"]")
  

-- generates data term for each name of a relationship and both ending entities
-- if it is of type (1 to 1), (N to 1), (1 to N) or (M to N)
getrelationtypes :: [Entity] -> Relationship -> String
getrelationtypes ents (Relationship 
                       "" 
                       [REnd e1Name _ _, REnd e2Name reName _]) =                    
                           "((\""++e1Name++ "\", \""++reName ++"\", \""
                                  ++(getCorEnt ents e1Name e2Name)++"\"), "
                            ++"( MtoN \"" ++ e2Name ++"\")),\n"++
                         (spaceN 7)++"((\""++e1Name++ "\", \""++(firstLow e2Name) ++"\", \""
                                 ++(getCorEnt ents e1Name e2Name)++"\"),"
                            ++"( MtoN \"" ++ e2Name ++"\"))"
getrelationtypes _ (Relationship 
                      rName 
                      [REnd e1Name re1Name (Exactly 1), 
                       REnd e2Name re2Name (Exactly 1)]) =                     
                           "((\""++e1Name++"\", \""++re1Name++"\", \""++e2Name++"\"),"
                                 ++"(OnetoOne \""++rName++"\")),\n"++
                          (spaceN 7)++"((\""++e2Name++"\", \""++re2Name++"\", \""++e1Name++"\"),"
                                 ++"(OnetoOne \""++rName++"\"))"  
getrelationtypes _ (Relationship 
                       rName 
                       [REnd e1Name re1Name (Between 0 _),
                         REnd e2Name re2Name (Exactly 1)]) =
                            "((\""++e2Name++"\", \""++re1Name++"\", \""++e1Name++"\"),"
                                                    ++"(OnetoN \""++rName++"\")),\n"++
                            (spaceN 7)++"((\""++e1Name++"\", \""++re2Name++"\", \""++e2Name++"\"),"
                                                    ++"(NtoOne \""++rName++"\"))"  
getrelationtypes _ (Relationship 
                     rName@(_:_) 
                     [REnd e1Name re1Name (Exactly 1),
                       REnd e2Name re2Name (Between 0 _)]) = 
                          "((\""++e2Name++"\", \""++re1Name++"\", \""++e1Name++"\"),"
                                             ++"(NtoOne \""++rName++"\")),\n"++
                          (spaceN 7)++"((\""++e1Name++"\", \""++re2Name++"\", \""++e2Name++"\"),"
                                            ++"(OnetoN \""++rName++"\"))"
getrelationtypes _ (Relationship
                      rName
                      [REnd e1Name re1Name (Between 0 _),
                       REnd e2Name re2Name (Between 0 (Max 1))]) =
                          "((\""++e2Name++"\", \""++re1Name++"\", \""++e1Name++"\"),"
                                                ++"(OnetoN \""++rName++"\")),\n"++
                          (spaceN 7)++"(\""++e1Name++"\", \""++re2Name++"\", \""++e2Name++"\"),"
                                                ++"(NtoOne \""++rName++"\"))"
getrelationtypes _ (Relationship
                      rName
                      [REnd e1Name re1Name (Between 0 (Max 1)),
                       REnd e2Name re2Name (Between 0 _)]) =
                          "((\""++e2Name++"\", \""++re1Name++"\", \""++e1Name++"\"),"
                                                ++"(NtoOne \""++rName++"\")),\n"++
                          (spaceN 7)++"(\""++e1Name++"\", \""++re2Name++"\", \""++e2Name++"\"),"
                                                ++"(OnetoN \""++rName++"\"))"

--finding second entity belonging to an MtoN relationship                                                
getCorEnt :: [Entity] -> String -> String -> String
getCorEnt [] _ _ = ""  -- this should not happen
getCorEnt ((Entity name attrs):ents) eName rName = 
  if name == rName 
   then checkAttributes attrs eName
   else getCorEnt ents eName rName
  where checkAttributes ((Attribute _ typ _ _):atts) n =
           case typ of
             (KeyDom kName) -> if kName == n 
                                 then checkAttributes atts n
                                 else kName
             _              -> checkAttributes atts n
        checkAttributes [] _ = "" --should not happen
 

-- generates data term providing for each attribute (name) if it is nullable or not
getNullableAttr :: Entity -> String
getNullableAttr (Entity name attrs) = 
     (intercalate (",\n"++(spaceN 7))
                  (map (getNullValue name) attrs)) 

getNullValue :: String -> Attribute -> String
getNullValue (e:name) a@(Attribute aName _ _ nullable) =  
        "(\"" ++ (toLower e):name ++ aName ++"\""
          ++", "++(show nullable)++")"

-- generates data term providing the type for each attribut
getAttrTypes :: Entity -> String
getAttrTypes (Entity name attrs) = 
    intercalate (",\n"++(spaceN 7)) 
                (map (getTypeOf name) attrs)
                                        
getTypeOf :: String -> Attribute -> String
getTypeOf (e:name) (Attribute aName domain key _) = 
 case domain of
    (IntDom _ ) -> case key of
                     PKey -> ("(\""++(toLower e):name 
                              ++aName++"\""++", \""
                              ++((toUpper e):name)++"\")")
                     NoKey -> ("(\""++(toLower e):name
                              ++aName++"\""++", \"int\")")
                     Unique -> ("(\""++(toLower e):name 
                               ++aName++"\""++", \"int\")")
    (FloatDom _ ) -> ("(\""++(toLower e):name ++aName++"\""
                      ++", \"float\")")
    (CharDom _ )  -> ("(\""++(toLower e):name ++aName++"\""
                       ++", \"char\")")
    (StringDom _ ) ->  ("(\""++(toLower e):name ++aName++"\""
                        ++", \"string\")")
    (BoolDom _ )   -> ("(\""++(toLower e):name ++aName++"\""
                       ++", \"bool\")")
    (DateDom _ ) -> ("(\""++(toLower e):name ++aName++"\""
                      ++", \"date\")")
    (KeyDom e2Name ) -> ("(\""++(toLower e):name ++aName++"\""
                          ++", \""++e2Name++"\")")
                             


-- generates data term providing for each tableName the list of its attributes
getAttrList :: Entity -> String
getAttrList (Entity name attrs) =
  "(\""++name++"\""++", ["
    ++ (intercalate ", " (map selectAttr attrs)) 
    ++"])"
  where selectAttr (Attribute name _ _ _) = "\""++name++"\""

-- ------- writing file containing all type needed for use of CDBI ------------

-- Write the data so CDBI can be used
writeEntityData :: Entity -> Handle -> IO ()
writeEntityData ent file = do writeDatatype ent file
                              writeID ent file
                              writeDescription ent file
                              writeTables ent file
                              writeColumns ent file
                              writeColumnDescriptions ent file
                              writeGetterSetters ent file
                              writeKeyToValueFunc ent file
                              hPutStrLn file ""                              
                              
-- Write a Entity-Datatype based on an Entity
writeDatatype :: Entity -> Handle -> IO ()
writeDatatype (Entity name (a:atr)) file = do
  let str = "data " ++ name ++ " = " ++ name ++ " " ++
            (foldl (\y x -> y ++ (writeAttributes x name))
                   (writeAttributes a name) atr)
  hPutStrLn file str

-- Write a ID-Datatype based on an Entity
writeID :: Entity -> Handle -> IO ()
writeID (Entity name _) file = do
  let str = "data " ++ name++"ID = " ++ name ++"ID Int"
  hPutStrLn file str

-- Write an Entity-Description based on an Entity
writeDescription :: Entity -> Handle -> IO ()
writeDescription (Entity name@(n:ns) (a:atr)) file = do
  let str = ((toLower n) : ns) ++ "Description = ED \"" ++ name ++
            "\"\n    "++ "[" ++
            (foldl (\y x -> y ++ ", " ++ (writeTypes x))
                   (writeTypes a) atr) ++
            "]\n    " ++ "(\\(" ++ name ++ " " ++
            (foldl (\y x -> y ++ " " ++ (writeTransFunOne x name))
                   (writeTransFunOne a name) atr) ++
            ") -> " ++ "[" ++
            (foldl (\y x -> y ++ ", " ++ (writeTransFunTwo x))
                   (writeTransFunTwo a) atr) ++
            "])\n    " ++ "(\\(" ++ name ++ " " ++
            (foldl (\y x -> y ++ " " ++ (writeTransFunOne x name))
                   (writeTransFunOne a name) atr) ++
             ") -> " ++ "[" ++
            (foldl (\y x -> y ++ ", " ++ (writeTransFunTwoTwo x))
                   (writeTransFunTwoTwo a) atr) ++ 
            "])\n    " ++"(\\[" ++
            (foldl (\y x -> y ++ ", " ++ (writeTransFunThree x))
                   (writeTransFunThree a) atr) ++
            "] -> " ++ name ++ " " ++
            (foldl (\y x -> y ++ " " ++ (writeTransFunFour x name))
                   (writeTransFunFour a name) atr) ++ ")"
  hPutStrLn file (((toLower n) : ns) ++ "Description :: EntityDescription " ++ name)
  hPutStrLn file str

-- Write a Table-Type based on an Entity
writeTables :: Entity -> Handle -> IO ()
writeTables (Entity name@(n:ns) _) file = do
  let str = ((toLower n):ns) ++ "Table :: Table"
  let str2 = ((toLower n):ns) ++ "Table = " ++ "\"" ++ name ++ "\""
  hPutStrLn file str
  hPutStrLn file str2

writeColumnDescriptions :: Entity -> Handle -> IO()
writeColumnDescriptions (Entity name atr) file = 
  do mapIO_ (\x -> (writeColumnDescription x name file)) atr

writeColumnDescription :: Attribute -> String -> Handle -> IO()
writeColumnDescription a@(Attribute atr dom _ _) name@(n:ns) file =
  do let str = ((toLower n):ns) ++ atr  ++ "ColDesc" 
     let typ = writeAttributes a name --if(atr == "Key") then name ++"ID" else getType dom
     let sqlTyp = writeTypes a
     hPutStrLn file (str ++ " :: ColumnDescription "++ typ )
     hPutStrLn file (str ++ " = ColDesc " ++ 
                              "\"\\\"" ++ name ++ "\\\"." ++ "\\\"" 
                                ++ atr ++ "\\\"\" "++ sqlTyp ++
                                 "\n    (\\"++(writeTransFunOne a name)
                                  ++" -> ("++ (writeTransFunTwo a) ++")) (\\("
                                  ++ (writeTransFunThree a)++") -> "
                                  ++(writeTransFunFour a name)++" )")


-- Write all needed Column-Datatypes based on an Entity
writeColumns :: Entity -> Handle -> IO ()
writeColumns (Entity name atr) file
             = do mapIO_ (\x -> (writeColumn x name file)) atr

-- Write a Column-Datatype from an Attribute
writeColumn :: Attribute -> String -> Handle -> IO ()
writeColumn a@(Attribute atr dom _ _) name@(n:ns) file =
    do let str = ((toLower n):ns) ++ "Column" ++ atr
       let typ = if(atr == "Key") then name ++"ID" else getType dom
       hPutStrLn file (str ++ " :: Column " ++ typ)
       hPutStrLn file (str ++ " = Column " ++ "\"\\\"" ++ atr ++ "\\\"\" " ++
                              "\"\\\"" ++ name ++ "\\\"." 
                               ++ "\\\"" ++ atr ++ "\\\"\" ")



-- Get the type of a Domain as a String
getType :: Domain -> String
getType (IntDom _) = "Int"
getType (FloatDom _) = "Float"
getType (CharDom _) = "Char"
getType (StringDom _) = "String"
getType (BoolDom _) = "Bool"
getType (DateDom _) = "ClockTime"
getType (KeyDom name) = name ++ "ID"

-- Write all getter and setter methods based on an Entity
writeGetterSetters :: Entity -> Handle -> IO ()
writeGetterSetters (Entity name atr) file = do
  mapIO_ (\x -> let index =
                      case (elemIndex x atr) of
                        (Just a) -> a
                        Nothing  -> 0
                in (writeGetterSetter x name file index (length atr))) atr

-- Write a getter and a setter methods based on an Attribute
writeGetterSetter :: Attribute -> String -> Handle -> Int -> Int -> IO ()
writeGetterSetter (Attribute name _ _ _) (a:b) conn ind len = do
  let str = ((toLower a):b) ++ name ++ " (" ++ (a:b) ++ " " ++
            (createUnderscores (ind + 1) (len-ind-1)) ++ ") = a"
  let str2 = "set" ++ (a:b) ++ name ++ " (" ++ (a:b) ++ " " ++
             (createParametersLeft (ind + 1) (len-ind-1)) ++ ") a = " ++
             "(" ++ (a:b) ++ " " ++
             (createParametersRight (ind + 1) (len-ind-1)) ++ ")"
  hPutStrLn conn str
  hPutStrLn conn str2

-- Auxiliary function for writeGetterSetter that creates the needed
-- amount of underscores and places the "a" at the correct position
createUnderscores :: Int -> Int -> String
createUnderscores ind len = case ind of
  0 -> case len of
         0 -> ""
         n -> "_ " ++ (createUnderscores 0 $ n-1)
  1 -> "a " ++ createUnderscores 0 len
  n -> "_ " ++ createUnderscores (n-1) len

-- Auxiliary function for writeGetterSetter that creates the needed
-- amount of parameters for setter-functions on the left side
createParametersLeft :: Int -> Int -> String
createParametersLeft ind len = case ind of
  0 -> case len of
         0 -> ""
         n -> "b" ++ (show n) ++ " " ++ (createParametersLeft 0 $ n-1)
  1 -> "_ " ++ createParametersLeft 0 len
  n -> "a" ++ (show n) ++ " " ++ createParametersLeft (n-1) len

-- Auxiliary function for writeGetterSetter that creates the needed amount
-- of parameters for setter-functions on the right side
createParametersRight :: Int -> Int -> String
createParametersRight ind len = case ind of
  0 -> case len of
         0 -> ""
         n -> "b" ++ (show n) ++ " " ++ (createParametersRight 0 $ n-1)
  1 -> "a " ++ createParametersRight 0 len
  n -> "a" ++ (show n) ++ " " ++ createParametersRight (n-1) len

-- Writes the left side of the first and second function in the entity-description
writeTransFunOne :: Attribute -> String -> String
writeTransFunOne (Attribute (a:b) dom NoKey _) _ = case dom of
  KeyDom c -> "("++ c ++ "ID " ++ ((toLower a):b) ++ ")"
  _        -> ((toLower a):b)

writeTransFunOne (Attribute (a:b) dom Unique _) _ = case dom of
  KeyDom c -> "("++ c ++ "ID " ++ ((toLower a):b) ++ ")"
  _        -> ((toLower a):b)

writeTransFunOne (Attribute (a:b) (KeyDom c) PKey _) _ =
  "("++ c ++ "ID " ++ ((toLower a):b) ++ ")"
writeTransFunOne (Attribute (a:b) (IntDom _) PKey _) name =
  "("++ name ++"ID " ++ ((toLower a):b) ++ ")"

-- Writes the right side of the first function in the entity-description
writeTransFunTwo :: Attribute -> String
writeTransFunTwo (Attribute (a:b) (IntDom _) _ False) =
  "SQLInt " ++ ((toLower a):b)
writeTransFunTwo (Attribute (a:b) (IntDom _) _ True) =
  "(sqlIntOrNull " ++ ((toLower a):b) ++ ")"
writeTransFunTwo (Attribute (a:b) (FloatDom _) _ False) =
  "SQLFloat " ++ ((toLower a):b)
writeTransFunTwo (Attribute (a:b) (FloatDom _) _ True) =
  "(sqlFloatOrNull " ++ ((toLower a):b) ++ ")"
writeTransFunTwo (Attribute (a:b) (CharDom _) _ False) =
  "SQLChar " ++ ((toLower a):b)
writeTransFunTwo (Attribute (a:b) (CharDom _) _ True) =
  "(sqlCharOrNull " ++ ((toLower a):b) ++ ")"
writeTransFunTwo (Attribute (a:b) (StringDom _) _ False) =
  "SQLString " ++ ((toLower a):b)
writeTransFunTwo (Attribute (a:b) (StringDom _) _ True) =
  "(sqlStringOrNull " ++ ((toLower a):b) ++ ")"
writeTransFunTwo (Attribute (a:b) (BoolDom _) _ False) =
  "SQLBool " ++ ((toLower a):b)
writeTransFunTwo (Attribute (a:b) (BoolDom _) _ True) =
  "(sqlBoolOrNull " ++ ((toLower a):b) ++ ")"
writeTransFunTwo (Attribute (a:b) (DateDom _) _ False) =
  "SQLDate " ++ ((toLower a):b)
writeTransFunTwo (Attribute (a:b) (DateDom _) _ True) =
  "(sqlDateOrNull " ++ ((toLower a):b) ++ ")"
writeTransFunTwo (Attribute (a:b) (KeyDom _) _ False) =
  "SQLInt " ++ ((toLower a):b)

--writes the right side of second function in entity describtion  
writeTransFunTwoTwo :: Attribute -> String
writeTransFunTwoTwo (Attribute name@(a:b) (IntDom _) _ False)  
   | name == "Key" = "SQLNull "
   | otherwise = "SQLInt " ++ ((toLower a):b)
writeTransFunTwoTwo (Attribute (a:b) (IntDom _) _ True) =
  "(sqlIntOrNull " ++ ((toLower a):b) ++ ")"
writeTransFunTwoTwo (Attribute (a:b) (FloatDom _) _ False) =
  "SQLFloat " ++ ((toLower a):b)
writeTransFunTwoTwo (Attribute (a:b) (FloatDom _) _ True) =
  "(sqlFloatOrNull " ++ ((toLower a):b) ++ ")"
writeTransFunTwoTwo (Attribute (a:b) (CharDom _) _ False) =
  "SQLChar " ++ ((toLower a):b)
writeTransFunTwoTwo (Attribute (a:b) (CharDom _) _ True) =
  "(sqlCharOrNull " ++ ((toLower a):b) ++ ")"
writeTransFunTwoTwo (Attribute (a:b) (StringDom _) _ False) =
  "SQLString " ++ ((toLower a):b)
writeTransFunTwoTwo (Attribute (a:b) (StringDom _) _ True) =
  "(sqlStringOrNull " ++ ((toLower a):b) ++ ")"
writeTransFunTwoTwo (Attribute (a:b) (BoolDom _) _ False) =
  "SQLBool " ++ ((toLower a):b)
writeTransFunTwoTwo (Attribute (a:b) (BoolDom _) _ True) =
  "(sqlBoolOrNull " ++ ((toLower a):b) ++ ")"
writeTransFunTwoTwo (Attribute (a:b) (DateDom _) _ False) =
  "SQLDate " ++ ((toLower a):b)
writeTransFunTwoTwo (Attribute (a:b) (DateDom _) _ True) =
  "(sqlDateOrNull " ++ ((toLower a):b) ++ ")"
writeTransFunTwoTwo (Attribute (a:b) (KeyDom _) _ _) =
  "SQLInt " ++ ((toLower a):b)


-- Writes the left side of the third function in the entity-description
writeTransFunThree :: Attribute -> String
writeTransFunThree (Attribute (a:b) (IntDom _) _ False) =
  "SQLInt " ++ ((toLower a):b)
writeTransFunThree (Attribute (a:b) (FloatDom _) _ False) =
  "SQLFloat " ++ ((toLower a):b)
writeTransFunThree (Attribute (a:b) (CharDom _) _ False) =
  "SQLChar " ++ ((toLower a):b)
writeTransFunThree (Attribute (a:b) (StringDom _) _ False) =
  "SQLString " ++ ((toLower a):b)
writeTransFunThree (Attribute (a:b) (BoolDom _) _ False) =
  "SQLBool " ++ ((toLower a):b)
writeTransFunThree (Attribute (a:b) (DateDom _) _ False) =
  "SQLDate " ++ ((toLower a):b)
writeTransFunThree (Attribute (a:b) (KeyDom _) _ False) =
  "SQLInt " ++ ((toLower a):b)
writeTransFunThree (Attribute (a:b) _ _ True) = ((toLower a):b)

-- Writes the right side of the third function in the entity-description
writeTransFunFour :: Attribute -> String -> String
writeTransFunFour (Attribute (a:b) (IntDom _) NoKey True) _ =
  "(intOrNothing " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (FloatDom _) NoKey True) _ =
  "(floatOrNothing " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (CharDom _) NoKey True) _ =
  "(charOrNothing " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (StringDom _) NoKey True) _ =
  "(stringOrNothing " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (BoolDom _) NoKey True) _ =
  "(boolOrNothing " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (DateDom _) NoKey True) _ =
  "(dateOrNothing " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (KeyDom c) NoKey True) _ =
  "("++ c ++ "ID " ++ "(intOrNothing " ++ ((toLower a):b) ++ "))"

writeTransFunFour (Attribute (a:b) (IntDom _) Unique True) _ =
  "(intOrNothing " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (FloatDom _) Unique True) _ =
  "(floatOrNothing " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (CharDom _) Unique True) _ =
  "(charOrNothing " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (StringDom _) Unique True) _ =
  "(stringOrNothing " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (BoolDom _) Unique True) _ =
  "(boolOrNothing " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (DateDom _) Unique True) _ =
  "(dateOrNothing " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (KeyDom c) Unique True) _ =
  "("++ c ++ "ID " ++ "(intOrNothing " ++ ((toLower a):b) ++ "))"

writeTransFunFour (Attribute (a:b) dom NoKey False) _ =
  case dom of
    (KeyDom d) -> "("++ d ++ "ID " ++ ((toLower a):b) ++ ")"
    _          -> (toLower a):b

writeTransFunFour (Attribute (a:b) dom Unique False) _ =
  case dom of
    (KeyDom d) -> "("++ d ++ "ID " ++ ((toLower a):b) ++ ")"
    _          -> (toLower a):b

writeTransFunFour (Attribute (a:b) (KeyDom c) PKey _) _ =
  "("++ c ++ "ID " ++ ((toLower a):b) ++ ")"
writeTransFunFour (Attribute (a:b) (IntDom _) PKey _) name =
  "("++ name ++"ID " ++ ((toLower a):b) ++ ")"

-- Write the Attributes as String to create the Entity-Datatypes
writeAttributes :: Attribute -> String -> String
writeAttributes (Attribute _ (IntDom _) _ True) _ = "(Maybe Int) "
writeAttributes (Attribute a (IntDom _) _ False) name = case a of
                                                            "Key" -> name++"ID "
                                                            _     -> "Int "
writeAttributes (Attribute _ (FloatDom _) _ True) _ = "(Maybe Float) "
writeAttributes (Attribute _ (FloatDom _) _ False) _ = "Float "
writeAttributes (Attribute _ (CharDom _) _ True) _ = "(Maybe Char) "
writeAttributes (Attribute _ (CharDom _) _ False) _ = "Char "
writeAttributes (Attribute _ (StringDom _) _ True) _ = "(Maybe String) "
writeAttributes (Attribute _ (StringDom _) _ False) _ = "String "
writeAttributes (Attribute _ (BoolDom _) _ True) _ = "(Maybe Bool) "
writeAttributes (Attribute _ (BoolDom _) _ False) _ = "Bool "
writeAttributes (Attribute _ (DateDom _) _ True) _ = "(Maybe ClockTime) "
writeAttributes (Attribute _ (DateDom _) _ False) _ = "ClockTime "
writeAttributes (Attribute _ (KeyDom k) _ True) _ = "(Maybe "++k++"ID) "
writeAttributes (Attribute _ (KeyDom k) _ False) _ = k++"ID "

-- Write Types as String to create the Entity-Descriptions
writeTypes :: Attribute -> String
writeTypes (Attribute _ (IntDom _) _ _) = "SQLTypeInt"
writeTypes (Attribute _ (FloatDom _) _ _) = "SQLTypeFloat"
writeTypes (Attribute _ (CharDom _) _ _) = "SQLTypeChar"
writeTypes (Attribute _ (StringDom _) _ _) = "SQLTypeString"
writeTypes (Attribute _ (BoolDom _) _ _) = "SQLTypeBool"
writeTypes (Attribute _ (DateDom _) _ _) = "SQLTypeDate"
writeTypes (Attribute _ (KeyDom _) _ _) = "SQLTypeInt"


writeKeyToValueFunc :: Entity -> Handle -> IO()
writeKeyToValueFunc (Entity name attr) file =
  case (head attr) of
     (Attribute "Key" _ PKey _ ) -> 
           hPutStrLn file ((firstLow name)++"ID :: "++name++"ID -> Value "
                           ++name++"ID\n"
                           ++(firstLow name)++"ID ("++name++
                           "ID key) = idVal key\n\n")
     _                           -> return ()

-- Creates a sqlite3-database (sqlite3 needs to be installed)
createDatabase :: [Entity] -> Handle -> IO ()
createDatabase ents db = mapIO_ (\ent -> (createDatabase' ent)) ents
  where createDatabase' (Entity name (a:atr)) =
          case a of
            (Attribute "Key" _ _ _) -> do hPutStrLn db ("create table '" ++ name ++ "'(" ++
                                                       (foldl (\y x -> y ++ " ," ++ (writeDBAttributes x))
                                                       (writeDBAttributes a) atr) ++ ");")
            _                       -> do let str = ("create table '" ++ name ++ "'(" ++
                                                    (foldl (\y x -> y ++ " ," ++ (writeDBRelationship x))
                                                    (writeDBRelationship a) atr) ++ ", primary key (" ++
                                                    (writePrimaryKey (a:atr)) ++ "));")
                                          hPutStrLn db str

-- Write Attribute for table-creation (Used when first Attribute
-- of Entity is named "Key" because the primary key will be that "Key" then)
writeDBAttributes :: Attribute -> String
writeDBAttributes (Attribute name ty key nullable) =
    "'" ++ name ++ "'" ++
            (case ty of
                 IntDom Nothing      -> ""
                 IntDom _      -> " int"
                 FloatDom _    -> " float"
                 CharDom _     -> " char"
                 StringDom _   -> " string"
                 BoolDom _     -> " boolean"
                 DateDom _     -> " string"
                 KeyDom str    -> " int " ++ "REFERENCES '" ++ str ++ "'(Key)") ++
            (case key of
                 PKey   -> " integer primary key"
                 Unique -> " unique"
                 NoKey  -> "") ++
            (case nullable of
                 True  -> ""
                 False -> if key == PKey then ""
                                         else " not null")

-- Same as writeDBAttributes but for the case that the first Attribute of
-- Entity is not named "Key", because there will be a combined primary key then
-- and the string needs to be built a little different then
writeDBRelationship :: Attribute -> String
writeDBRelationship (Attribute name ty key nullable) =
  "'" ++  name ++ "'" ++
            (case ty of
                 IntDom _    -> " int"
                 FloatDom _  -> " float "
                 CharDom _   -> " char "
                 StringDom _ -> " string "
                 BoolDom _   -> " boolean "
                 DateDom _   -> " string "
                 KeyDom str    -> " int " ++ "REFERENCES '" ++ str ++"'(Key)")
         ++ (case key of
                 Unique -> " unique"
                 _  -> "")
         ++ (case nullable of
                 True  -> ""
                 False -> " not null")

-- Write a combined primary key
writePrimaryKey :: [Attribute] -> String
writePrimaryKey ((Attribute name _ PKey _):atr) =
  "'" ++ name ++ "'" ++
  (case (writePrimaryKey atr) of
     "" -> ""
     x  -> ", " ++ x)
writePrimaryKey ((Attribute _ _ Unique _):atr) = writePrimaryKey atr
writePrimaryKey ((Attribute _ _ NoKey _):atr) = writePrimaryKey atr
writePrimaryKey [] = ""

   
firstLow :: String -> String
firstLow [] = []
firstLow (s:str) = (toLower s):str

spaceN :: Int -> String
spaceN n | n == 0 = ""
         | otherwise = ' ':(spaceN (n-1))
