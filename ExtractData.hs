{-|
Module : ExtractData
Description : Contains all relevant methods. Reads the .xml file and extracts
the information to a .CSV file
Copyright : Simón Fallon
This module contains the main function extData and the other methods that were
implemented in order to extract the infoboxes information from the xml file.
This module reads the .xml file to a string and then calls extData to extract
the information from the infoboxes of the Wikipedia articles. It then stores it
in a new file named "infoboxData.csv".
-}

module ExtractData where
import Data.CSV
import Text.Regex.TDFA
import System.IO
import Data.List.Split

{-|
 The method cutEnds matches from an alphabetic character until it finds
 a different character type. It is used tu avoid characters like "[,]"
-}
cutEnds :: String -> String
cutEnds s = a
  where
    a = (s =~ "[A-Z](.*)[a-z]") :: String

{-|
 The method cutAndJoin takes a list of strings which contains the birth place
 or the death place. It converts this list to the wished format and then
 returns it
-}
cutAndJoin :: [String]-> String
cutAndJoin [] = ""
cutAndJoin (x:xs) = cutEnds x ++ "/" ++ cutAndJoin xs

{-|
 The method getName tries to match an expression which contains the name1 in a
 String and returns the result of the matching.
-}
getName :: String -> String
getName s = a
  where
    a = (s =~ "(.*)</title>") :: String

{-|
 The
-}
cleanName :: String -> String
cleanName s = a
  where
    a = (s =~ "([[:alpha:][:blank:]]+)") :: String

{-|
The method getBirthD tries to match an expression which contains the birth date
in a String and returns the result of the matching.
-}
getBirthD :: String -> String
getBirthD s = a
  where
    a = (s =~ "\\|( )*birth_date(.*)") :: String

{-|
 The method cleanDate takes the Strings that were returned by getBirthD or
 getDeathD and converts the information to the wished format which then returns.
-}
cleanDate :: String -> String
cleanDate s = res
  where
    a = (s =~ "\\=(.*)") :: String
    ne = (a=~ "[0-9]") :: Bool
    df = (a=~ "df") :: Bool
    date = (s =~ "[0-9]{4}\\|[0-9]+\\|[0-9]+") :: String
    y = (date =~ "[0-9]{4}") :: String
    b = (date =~ "\\|[0-9]+\\|[0-9]+") :: String
    m = (b =~ "[0-9]+") :: String
    d = (b =~ "[0-9]+$") :: String
    res = if ne then (if df then y++"/"++d++"/"++m else y++"/"++m++"/"++d) else ""

{-|
The method getDeathD tries to match an expression which contains the death date
in a String and returns the result of the matching.
-}
getDeathD :: String -> String
getDeathD s = a
  where
    a = (s =~ "\\|( )*death_date(.*)") :: String

{-|
The method getBirthP tries to match an expression which contains the birth place
in a String and returns the result of the matching.
-}
getBirthP :: String -> String
getBirthP s = a
  where
    a = (s =~ "\\|( )*birth_place(.*)") :: String

{-|
 The method cleanPlace takes the Strings that were returned by getBirthP or
 getDeathP and converts the information to the wished format which then returns.
-}
cleanPlace :: String -> String
cleanPlace s = d
  where
    a = (s =~ "\\=(.*)") :: String
    b = (a =~ "[A-Za-z](.*)[A-Za-z]") :: String
    c = (b =~ "\\,") :: Bool
    e = splitOn "," b
    d = if c then init (cutAndJoin(splitOn "," b)) else b

{-|
The method getDeathP tries to match an expression which contains the death place
in a String and returns the result of the matching.
-}
getDeathP :: String -> String
getDeathP s = a
  where
    a = (s =~ "\\|( )*death_place(.*)") :: String

{-|
The method getAlmaM tries to match an expression which contains the alma mater
in a String and returns the result of the matching.
-}
getAlmaM :: String -> String
getAlmaM s = a
  where
    a = (s =~ "\\|( )*alma_mater(.*)") :: String

{-|
 The method cleanVarious takes the Strings that were returned by getAlmaM or
 getFields and converts the information to the wished format which then returns.
 In the case it finds multiple data it returns "Various".
-}
cleanVarious :: String -> String
cleanVarious s = d
  where
    a = (s =~ "\\=(.*)") :: String
    b = (a =~ "[A-Za-z](.*)[a-z]") :: String
    c = (b =~ "\\,|\\]") :: Bool
    d = if c then "Various" else b

{-|
The method getFields tries to match an expression which contains the fields in
a String and returns the result of the matching.
-}
getFields :: String -> String
getFields s = a
  where
    a = (s =~ "\\|( )*field(.*)") :: String


{-|
 The method ext takes a string that represents an entire article and extracts
 the information of the infobox calling the previous methods. It returns the
 information in form of a list of lists of strings.
-}
ext :: String -> [[String]]
ext s = res
  where
    a = cleanName (getName s)
    b = cleanDate (getBirthD s)
    c = cleanDate (getDeathD s)
    d = cleanPlace (getBirthP s)
    e = cleanPlace (getDeathP s)
    f = cleanVarious (getAlmaM s)
    g = cleanVarious (getFields s)
    res = [[a]++[b]++[c]++[d]++[e]++[f]++[g]]

{-|
 The method extData takes a list of Strings containing every article and
 extracts the information of each one, recursively appending every result to
 gather the information.
-}
extData :: [String] -> [[String]]
extData xs = foldr ((++) . ext) [[]] xs


{-|
 The method main reads the .xml file and extracts the information calling
 extData to then store it in a new file "infoboxData.csv"
-}
main:: IO ()
main = do
    datos <- readFile "data.xml"
    let a = extData(tail (splitOn "<title>" datos))
    writeFile "infoboxData.csv" (genCsvFile a)
    let oe = cleanPlace (getBirthP datos)
    print a
    return ()
