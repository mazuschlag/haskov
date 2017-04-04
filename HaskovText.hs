module HaskovText where

import Data.Char (isUpper)
import Data.Text (Text, pack, unpack, splitOn, concat, find)
import qualified Data.Text as Tex
import Data.Map.Strict (Map, (!), lookup, fromList, size)
import qualified Data.Map.Strict as Map

import Haskov (fromMap, walk)
import qualified Haskov as Has

type TextMap = Map (Text, Text) Double

generate :: [Text] -> Text -> TextMap -> TextMap
generate [] prev tm = Map.insertWith (+) (prev, prev) 0 tm
generate (curr:tl) prev tm = generate tl curr (Map.insertWith (+) (prev, curr) 1 tm)

isStartWord :: Text -> Bool
isStartWord word = if isUpper (Tex.head word) then True else False

isEndWord :: Text -> Bool
isEndWord word = if find isEnd word == Nothing then False else True

isEnd :: Char -> Bool
isEnd '.' = True
isEnd '!' = True
isEnd '?' = True
isEnd _   = False