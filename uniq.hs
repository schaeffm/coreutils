module Main where

-- uniq where the ordering of lines is completely ignored
-- equivalent to sort | uniq
-- supports the majority of the GNU util's options
--
-- internally, we use a hashmap to count lines after they've been processed
-- flags either control output, filtering before output, or modification
-- before input

import           Data.Char          (isSpace, toLower)
import           Data.List          (sortOn)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe)
import           Data.Ord           (Down (..))
import           System.Directory   (doesFileExist)
import           System.Environment (getArgs)
import           Text.Printf        (printf)

helpText =
    ["uniq [option ...] [files ...]"
    ,""
    ,"  -h     show this help"
    ,"  -c     prefix lines with the number of occurences"
    ,"  -u     only show unique lines"
    ,"  -d     only show repeated lines"
    ,"  -i     do not consider case while making a match"
    ,"  -s n   do not compare the first n characters"
    ,"  -w n   only compare the first n characters"
    ]

type WordMap = Map.Map String Integer

singleton = Map.fromList [] :: WordMap

increment map word = Map.alter add word map
  where
        add :: Maybe Integer -> Maybe Integer
        add Nothing  = Just 1
        add (Just i) = Just $ i + 1

mutateContent :: [Flag] -> String -> [String]
mutateContent flags content =
  case flags of
    (Ignore  : fs) -> mutateContent fs $ map toLower content
    (Skip  n : fs) -> mutateContent fs $ apply (drop n) content
    (First n : fs) -> mutateContent fs $ apply (take n) content
    (_       : fs) -> mutateContent fs content
    []             -> map (dropWhile isSpace) $ lines content
  where apply f ls = unlines $ map f $ lines ls

mutateMap :: [Flag] -> WordMap -> WordMap
mutateMap flags map
  | Unique `elem` flags = Map.filter (== 1) map
  | Repeat `elem` flags = Map.filter (/= 1) map
  | otherwise           = map

data Flag = Count | Ignore | Unique | Repeat | Skip Int | First Int
    deriving (Eq)

countWords flags content
  | Count `elem` flags  = mapM_ printCount list
  | otherwise           = mapM_ printPlain list
  where
        list = extract $ mutateMap flags map
        map = foldl increment singleton $ mutateContent flags content

        extract :: WordMap -> [(String, Integer)]
        extract = sortOn (Down . snd) . Map.toList

        printCount (s, i) = putStrLn $ printf "% 5d %s" i s
        printPlain (s, _) = putStrLn s

handle :: [Flag] -> [String] -> Maybe String -> IO ()
handle flags arguments content =
    case arguments of
      ("-h" : _   ) -> mapM_ putStrLn helpText
      ("-c" : args) -> handle (Count:flags)  args content
      ("-u" : args) -> handle (Unique:flags) args content
      ("-d" : args) -> handle (Repeat:flags) args content
      ("-i" : args) -> handle (Ignore:flags) args content

      ("-s" : n : args) -> handle (Skip (read n :: Int):flags) args content
      ("-w" : n : args) -> handle (First (read n :: Int):flags) args content

      (file : args) -> do
        exists <- doesFileExist file
        if exists
          then do
            fileContent <- readFile file
            handle flags args $ Just (fileContent ++ fromMaybe "" content)

          else putStrLn $
            "uniq: " ++ file ++ ": No such file or directory"

      [] -> output content

  where output :: Maybe String -> IO ()
        output Nothing = do
            stdinContent <- getContents
            countWords flags stdinContent

        output (Just c) = countWords flags c

main :: IO ()
main = do
    args <- getArgs
    handle [] args Nothing
