{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- wc, word count
--
--  -l lines
--  -w words
--  -c chars

import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import           Data.Maybe           (fromMaybe)
import           GHC.Word
import           System.Directory     (doesFileExist)
import           System.Environment   (getArgs)
import           System.Exit          (die)

big = "/mnt/wss/home/working/data/generated/k.data"
medium = "/mnt/wss/home/working/data/generated/d.data"
small = "/mnt/wss/home/working/data/generated/a.data"

data Count = Count {
    _chars :: !Int,
    _words :: !Int,
    _lines :: !Int
    }
    deriving (Show)

wc :: FilePath -> IO Count
wc path = collect path >>= pure . (counter $ Count 0 0 0)

foo :: L.ByteString -> Count
foo cs = L.foldlChunks count base cs
    where
        base = Count 0 0 0
        count (Count c w l) xs = Count (c + 1) w (l + c_lines)
            where
                !c_lines = S.count 10 xs

lazyWords :: L.ByteString -> [L.ByteString]
lazyWords xs = case L.dropWhile isSpace xs of
                   "" -> []
    where
        isSpace 10 = True
        isSpace 32 = True
        isSpace _  = False

collect :: FilePath -> IO [GHC.Word.Word8]
collect path = L.readFile path >>= pure . L.unpack

counter :: Count -> [GHC.Word.Word8] -> Count
counter count []                  = count
counter (!Count c w l) (10:xs)    = counter (Count (c + 1) w (l + 1)) xs
counter (!Count c w l) (32:32:xs) = counter (Count (c + 1) w l) xs
counter (!Count c w l) (32:xs)    = counter (Count (c + 1) (w + 1) l) xs
counter (!Count c w l) (_:xs)     = counter (Count (c + 1) w l) xs

main :: IO ()
main = undefined
