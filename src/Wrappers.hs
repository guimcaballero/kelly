module Wrappers (wrapInBlue, wrapInYellow, wrapInPercent) where

wrapInYellow :: String -> String
wrapInYellow string = "\x001b[33m"++string ++ "\x001b[0m"

wrapInBlue :: String -> String
wrapInBlue string = "\x001b[36m"++string ++ "\x001b[0m"

wrapInPercent :: String -> String
wrapInPercent text = "%" ++ text ++ "%"
