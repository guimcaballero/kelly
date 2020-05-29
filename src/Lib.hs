module Lib
    ( chat
    ) where

data State = State { name::String } deriving (Show)


chat :: IO ()
chat = do
    putStrLn "Hello! I'm not Zeve!"
    _name <- askName
    putStrLn ("Hello " ++ _name)
    putStrLn "How are you today?"
    mainLoop (State _name)

mainLoop :: State -> IO ()
mainLoop state = do
    input <- getLine
    if null input || input == "quit" then do
        putStrLn "Bye!"
        return ()
    else do
        let answer = findAnswer input
        putStrLn  $ processAnswer answer state
        mainLoop state

askName :: IO String
askName = do
    putStrLn "What's your name?"
    _name <- getLine
    if null _name then
       askName
    else
       return _name

findAnswer :: String -> String
findAnswer text = "You said: " ++ text


processAnswer :: String -> State -> String
processAnswer text state = text ++ ". Your name is " ++ name state
-- TODO Change this into something that replaces variables
