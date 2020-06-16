{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( chat
    ) where

import Data.Text(pack, unpack, replace)
import Database.SQLite.Simple

data State = State { currentTopic::Int, name::String } deriving (Show)

data KnowledgeUnit = KnowledgeUnit { unitId::Int, userInput::String, kellyOutput::String, topic::Int, gotoTopic::Int } deriving (Show)

instance FromRow KnowledgeUnit where
  fromRow = KnowledgeUnit <$> field <*> field <*> field <*> field <*> field
instance ToRow KnowledgeUnit where
  toRow (KnowledgeUnit id' userInput' kellyOutput' topic' gotoTopic') = toRow (id', userInput', kellyOutput', topic', gotoTopic')


chat :: IO ()
chat = do
    putStrLn "Hello! I'm Kelly!"
    _name <- askName
    let state = State 1 _name
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
        answer <- findAnswer input state
        putStrLn $ "Z: " ++ processAnswer ( kellyOutput answer ) state
        mainLoop $ changeTopic (unitId answer) state

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

findAnswer :: String -> State -> IO KnowledgeUnit
findAnswer text state = do
  conn <- open "database.sqlite"
  exactKus <- query_ conn $ ( Query . pack ) ("SELECT * from knowledge_units where userInput='" ++ text ++ "' and topic=" ++ show (currentTopic state)) :: IO [KnowledgeUnit]
  likeKus <- query_ conn $ ( Query . pack ) ("SELECT * from knowledge_units where userInput like '%" ++ text ++ "%' and topic=" ++ show (currentTopic state)) :: IO [KnowledgeUnit]
  exactKusInGeneral <- query_ conn $ ( Query . pack ) ("SELECT * from knowledge_units where userInput='" ++ text ++ "' and topic=1") :: IO [KnowledgeUnit]
  likeKusInGeneral <- query_ conn $ ( Query . pack ) ("SELECT * from knowledge_units where userInput like '%" ++ text ++ "%' and topic=1") :: IO [KnowledgeUnit]
  close conn
  return $ returnAnswerFromSearch ( exactKus ++ likeKus ++ exactKusInGeneral ++ likeKusInGeneral )

returnAnswerFromSearch :: [KnowledgeUnit] -> KnowledgeUnit
returnAnswerFromSearch (ku:_) = ku
returnAnswerFromSearch [] = KnowledgeUnit (-1) "" "Sorry &&name, I'm afraid I couldn't understand you." 1 (-1)

processAnswer :: String -> State -> String
processAnswer text state = replaceName state (replaceCurrentTopic state text)

changeTopic :: Int -> State -> State
changeTopic (-1) state = state
changeTopic newTopic state = State newTopic (name state)

replaceName :: State -> String -> String
replaceName state text = replaceVar state text "&&name" ( Left . name )

replaceCurrentTopic :: State -> String -> String
replaceCurrentTopic state text = replaceVar state text "&&currentTopic" ( Right . currentTopic )

replaceVar :: State -> String -> String -> (State -> Either String Int) -> String
replaceVar state text var f = unpack $ replace (pack var) value (pack text)
    where value = pack $ showM val
          val = f state

showM :: Either String Int -> String
showM x = case x of
    Left y -> y
    Right y -> show y
