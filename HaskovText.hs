module HaskovText where

import System.IO
import System.IO.Error
import System.Environment
import System.Random

import Data.Char (isUpper)
import Data.Text (Text, pack, unpack, splitOn, concat, find)
import qualified Data.Text as Tex
import Data.Map.Strict (Map, (!), lookup, fromList, size)
import qualified Data.Map.Strict as Map

import Haskov (fromMap, walk)
import qualified Haskov as Has

type TextMap = Map (Text, Text) Double

main = fromIO `catchIOError` handler

fromIO :: IO()
fromIO = do
    (input:_) <- getArgs
    contents <- readFile input
    gen <- getStdGen

    let text = Tex.words (pack contents)
        tm = generate (tail text) (head text) Map.empty
        haskov1 = fromMap tm
    --haskov2 = Has.hmatrix haskov1
    chain <- walk haskov1 10
    --putStrLn $ show . Has.size $ haskov
    --putStrLn $ show chain
    --putStrLn $ show haskov1
    --putStrLn $ show (Has.statesI haskov1)
    putStrLn $ show . Has.steadyState $ haskov1
    putStrLn $ show chain

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

handler :: IOError -> IO()
handler e
    | isDoesNotExistError e = putStrLn "Parse Error: file does not exist"
    | isUserError         e = putStrLn "Parse Error: user error on input"
    | isAlreadyInUseError e = putStrLn "Parse Error: file already in use"
    | otherwise             = ioError e
