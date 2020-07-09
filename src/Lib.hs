{-# LANGUAGE OverloadedStrings #-}

module Lib (chat) where
import Wrappers
import Data.Text (pack, replace, unpack)

import Database.SQLite.Simple

import System.IO

data State = State{currentTopic :: Int, name :: String}
               deriving (Show)

data KnowledgeUnit = KnowledgeUnit{unitId :: Int, userInput :: String,
                                   kellyOutput :: String, topic :: Int,
                                   gotoTopic :: Int}
                       deriving (Show)

instance FromRow KnowledgeUnit where
        fromRow
          = KnowledgeUnit <$> field <*> field <*> field <*> field <*> field

instance ToRow KnowledgeUnit where
        toRow (KnowledgeUnit id' userInput' kellyOutput' topic' gotoTopic')
          = toRow (id', userInput', kellyOutput', topic', gotoTopic')

chat :: IO ()
chat
  = do putStrLn $ wrapInBlue "Kelly: " ++ "Hello! I'm Kelly!"
       _name <- askName
       let state = State 1 _name
       putStrLn $
         wrapInBlue "Kelly: " ++
           processAnswer "Hello &&name! How are you today?" state
       mainLoop state

mainLoop :: State -> IO ()
mainLoop state
  = do input <- promptLine (wrapInYellow "You: ")
       if null input || input == "quit" then
         do putStrLn "Bye!"
            return ()
         else
         do answer <- findAnswer input state
            putStrLn $
              wrapInBlue "Kelly: " ++ processAnswer (kellyOutput answer) state
            mainLoop $ changeTopic (unitId answer) state

askName :: IO String
askName
  = do putStrLn $ wrapInBlue "Kelly: " ++ "What's your name?"
       _name <- promptLine (wrapInYellow "Name: ")

       if null _name then askName else return _name

promptLine :: String -> IO String
promptLine prompt
  = do putStr prompt
       hFlush stdout
       getLine

findAnswer :: String -> State -> IO KnowledgeUnit
findAnswer text state
  = do conn <- open "database.sqlite"
       exactKus <- query conn
                      "SELECT * from knowledge_units where userInput=? and topic=?"
                      (text :: String, show (currentTopic state) :: String)
                     :: IO [KnowledgeUnit]
       likeKus <- query conn
                    "SELECT * from knowledge_units where userInput like ? and topic=?"
                    (text :: String, show (currentTopic state) :: String)
                    :: IO [KnowledgeUnit]
       exactKusInGeneral <- query conn
                              "SELECT * from knowledge_units where userInput=? and topic=1"
                              (Only (wrapInPercent text :: String))
                              :: IO [KnowledgeUnit]
       likeKusInGeneral <- query conn
                             "SELECT * from knowledge_units where userInput like ? and topic=1"
                             (Only (wrapInPercent text :: String))
                             :: IO [KnowledgeUnit]
       close conn
       return $
         returnAnswerFromSearch
           (exactKus ++ likeKus ++ exactKusInGeneral ++ likeKusInGeneral)

returnAnswerFromSearch :: [KnowledgeUnit] -> KnowledgeUnit
returnAnswerFromSearch (ku : _) = ku
returnAnswerFromSearch []
  = KnowledgeUnit (-1) "" "Sorry &&name, I'm afraid I couldn't understand you."
      1
      (-1)

processAnswer :: String -> State -> String
processAnswer text state = replaceAll text
  where replaceNameWithState = replaceName state
        replaceCurrentTopicWithState = replaceCurrentTopic state
        replaceAll = replaceCurrentTopicWithState . replaceNameWithState

changeTopic :: Int -> State -> State
changeTopic (-1) state = state
changeTopic newTopic state = State newTopic (name state)

replaceName :: State -> String -> String
replaceName state text = replaceVar state text "&&name" (Left . name)

replaceCurrentTopic :: State -> String -> String
replaceCurrentTopic state text
  = replaceVar state text "&&currentTopic" (Right . currentTopic)

replaceVar ::
           State -> String -> String -> (State -> Either String Int) -> String
replaceVar state text var f = unpack $ replace (pack var) value (pack text)
  where value = pack $ showM val
        val = f state

showM :: Either String Int -> String
showM (Left x) = x
showM (Right y) = show y
