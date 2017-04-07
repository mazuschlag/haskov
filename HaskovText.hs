module HaskovText where

import Data.Char (isUpper)
import Data.Text (Text, find, pack, splitOn)
import qualified Data.Text as Tex
import Haskov (Markov, insertWith)
import qualified Haskov as Has

generate :: Text -> Markov Text
generate text = 
    let textList = splitOn (pack " ") text
    in  if textList == [] 
        then Has.empty
        else generator textList (head textList) Has.empty

generator :: [Text] -> Text -> Markov Text -> Markov Text
generator [] prev has = insertWith (+) prev prev 0 has
generator (curr:tl) prev has = generator tl curr (insertWith (+) prev curr 1 has)

isStartWord :: Text -> Bool
isStartWord word = if isUpper (Tex.head word) then True else False

isEndWord :: Text -> Bool
isEndWord word = if find isEnd word == Nothing then False else True

isEnd :: Char -> Bool
isEnd '.' = True
isEnd '!' = True
isEnd '?' = True
isEnd _   = False