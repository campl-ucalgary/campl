{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module ServiceConstants where

import Data.Word

{-
    Magic constants for networking
-}

getRequest :: String
getRequest = "1"

putRequest :: String
putRequest = "2"

closeRequest :: String
closeRequest = "3"

hCaseIxGet :: Word
hCaseIxGet = 1

hCaseIxPut :: Word
hCaseIxPut = 2

hCaseIxClose :: Word
hCaseIxClose = 3 

pattern IxGet <- ((==hCaseIxGet) -> True)
pattern IxPut <- ((==hCaseIxPut) -> True)
pattern IxClose <- ((==hCaseIxClose) -> True)

invalidGetRequest :: String
invalidGetRequest = "4"

validGetRequest :: String
validGetRequest = "5"

