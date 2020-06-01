{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( chat
    ) where

import Data.Text(pack, unpack, replace)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data State = State { currentTopic::String, name::String } deriving (Show)

data KnowledgeUnit = KnowledgeUnit { id::Int, input::String, output::String, topic::Int, gotoTopic::Int } deriving (Show)

instance FromRow KnowledgeUnit where
  fromRow = KnowledgeUnit <$> field <*> field

chat :: IO ()
chat = do
    putStrLn "Hello! I'm not Zeve!"
    _name <- askName
    let state = State "General" _name
    putStrLn $ processAnswer "Hello &&name!" state
    putStrLn "How are you today?"
    mainLoop state

mainLoop :: State -> IO ()
mainLoop state = do
    input <- promptLine "You: "
    if null input || input == "quit" then do
        putStrLn "Bye!"
        return ()
    else do
        let answer = findAnswer input
        putStrLn $ "Z: " ++ processAnswer answer state
        mainLoop state

askName :: IO String
askName = do
    putStrLn "What's your name?"
    _name <- promptLine "Name: "
    if null _name then
       askName
    else
       return _name

promptLine :: String -> IO String
promptLine prompt = do
    putStr prompt
    getLine


getKUs :: IO ()
getKUs = do
  conn <- open "test.db"
  execute conn "INSERT INTO test (str) VALUES (?)"
    (Only ("test string 2" :: String))
  r <- query_ conn "SELECT * from test" :: IO [KnowledgeUnit]
  mapM_ print r
  close conn






findAnswer :: String -> String
findAnswer text = "Hello &&name!"

processAnswer :: String -> State -> String
processAnswer text state = replaceName state (replaceCurrentTopic state text)

replaceName :: State -> String -> String
replaceName state text = replaceVar state text "&&name" name

replaceCurrentTopic :: State -> String -> String
replaceCurrentTopic state text = replaceVar state text "&&currentTopic" currentTopic


replaceVar :: State -> String -> String -> (State -> String) -> String
replaceVar state text var f = unpack $ replace (pack var) (pack $ f state) (pack text)
