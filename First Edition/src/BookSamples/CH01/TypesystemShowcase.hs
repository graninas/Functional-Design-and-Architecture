module Main where

import qualified Data.Map as Map

toFloat :: Int -> Float
toFloat value = fromIntegral value

writeFloat :: Float -> IO ()
writeFloat value = writeFile "value.txt" (show value)

toFloatAndWrite :: Int -> IO ()
toFloatAndWrite value = let 
    value = toFloat 42
    in writeFloat value


data User = User String String
type Repository = Map.Map Int User

getUser :: Int -> Repository -> Maybe User
getUser key r = Map.lookup key r

getUserName :: User -> Maybe String
getUserName (User "" _) = Nothing
getUserName (User name _) = Just name

users = Map.fromList [ (0, User "" "")
                     , (1, User "Bill" "bill@gmail.com")]

getUserInitials :: Int -> Maybe Char
getUserInitials key =
    case getUser key users of
        Nothing -> Nothing
        Just user -> case getUserName user of
            Nothing -> Nothing
            Just name -> Just (head name)

printUserInitials :: Int -> IO ()
printUserInitials key = do
    let mbInitials = getUserInitials key
    maybe (return ()) print mbInitials

getUserInitials' u = do
    user <- getUser u users
    name <- getUserName user
    Just (head name)

