{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import qualified Data.Text as T
import Debug.Trace (traceShowM)
import System.Environment (getArgs)

type Hints = [Int]
data Field = Unknown | Empty | Marked
    deriving (Show, Eq)

data Board = Board { xhints :: [Hints]
                   , yhints :: [Hints]
                   , fields :: [[Field]]
                   , xdim   :: Int
                   , ydim   :: Int
                   }
        deriving (Show, Eq)

loadFile :: FilePath -> IO Board
loadFile fileName = do ls <- lines <$> readFile fileName
                       let (xhs, _ : yhs) = break (== "---") ls
                       let (xd, yd) = (length xhs, length yhs)
                       let fs = replicate yd $ replicate xd Unknown
                       return Board { xhints = map readHint xhs, yhints = map readHint yhs, fields = fs, xdim = xd, ydim = yd }
        where readHint :: String -> [Int]
              readHint = map (read . T.unpack) . T.splitOn (T.pack " ") . T.pack


processFile :: FilePath -> IO ()
processFile fileName = do b <- loadFile fileName
                          traceShowM b
                          return ()

main :: IO ()
main = do files <- getArgs
          mapM_ processFile files
