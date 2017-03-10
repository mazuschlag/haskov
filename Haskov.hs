module Haskov where

import System.IO
import System.IO.Error
import System.Environment
import System.Random

import Data.Char (isUpper)
import Data.List (group, sort)
import Data.Text (Text, pack, unpack, empty, splitOn, concat, find)
import qualified Data.Text as Text
import Data.Map.Strict (Map, (!), lookup, fromList, size)
import qualified Data.Map.Strict as Map

main = fromIO `catchIOError` handler

fromIO :: IO()
fromIO = do
    (input:_) <- getArgs
    contents <- readFile input
    gen <- getStdGen

    let text = Text.words (pack contents)
        keys = getkeys text
        haskovMap = generate text keys
        startWords = foldr (\x acc -> if isStartWord x then x:acc else acc) [] keys
        startIndex = randomR (0, (length startWords) - 1) gen :: (Int, StdGen)
        firstWord = startWords !! (fst startIndex)

    putStrLn $ unpack . Text.concat $ walk haskovMap firstWord (snd startIndex)

walk :: Map Text [Text] -> Text -> StdGen -> [Text]
walk markovMap key gen
    | key == empty = []
    | isEndWord key = key : []
    | otherwise = (key : (pack " ") : walk markovMap word (snd r))
    where
        r = randomR (0, (length (markovMap ! key)) - 1) gen :: (Int, StdGen)
        word = (markovMap ! key) !! (fst r)

generate :: [Text] -> [Text] -> Map Text [Text]
generate lists keys = fromList (map makeKeyChain keys)
    where makeKeyChain key = (key, (chain key empty lists))

chain :: Text -> Text -> [Text] -> [Text]
chain _ _ [] = []
chain key curr (next:list)
    | curr == key = next : chain key next list
    | otherwise   = chain key next list

getkeys :: [Text] -> [Text]
getkeys lists = map head . group . sort $ lists

isStartWord :: Text -> Bool
isStartWord word = if isUpper (Text.head word) then True else False

isEndWord :: Text -> Bool
isEndWord word = if find isEnd word == Nothing then False else True

isEnd :: Char -> Bool
isEnd '.' = True
isEnd '!' = True
isEnd '?' = True
isEnd _   = False

handler :: IOError -> IO()
handler e
    | isDoesNotExistError e = putStrLn "Parse Error: file does not exist"
    | isUserError         e = putStrLn "Parse Error: user error on input"
    | isAlreadyInUseError e = putStrLn "Parse Error: file already in use"
    | otherwise             = ioError e
