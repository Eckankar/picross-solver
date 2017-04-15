{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Data.List (transpose)
import qualified Data.Text as T
import Debug.Trace (traceShowM)
import System.Environment (getArgs)

type Hints = [Int]
data Field = Unknown | Empty | Marked
    deriving (Eq)

instance Show Field where
    show Unknown = " "
    show Empty   = "."
    show Marked  = "#"

data Board = Board { xhints :: [Hints]
                   , yhints :: [Hints]
                   , fields :: [[Field]]
                   , xdim   :: Int
                   , ydim   :: Int
                   }
        deriving (Eq)

instance Show Board where
    show b = unlines $ alignRight $
                map (++" ") xh ++
                zipWith (++) ("" : yh ++ [""]) flines
        where formatHints :: Hints -> String
              formatHints = unwords . map show

              padLeft n s = replicate d ' ' ++ s
                where d = n - length s

              alignRight :: [String] -> [String]
              alignRight ss = map (padLeft ml) ss
                where ml = maximum $ map length ss

              blines = map (concatMap show) $ fields b
              flines = ["┌" ++ replicate (xdim b) '─' ++ "┐"] ++
                       map (\l -> "│" ++ l ++ "│") blines ++
                       ["└" ++ replicate (xdim b) '─' ++ "┘"]

              yh :: [String]
              yh = alignRight $ map formatHints $ yhints b

              xh :: [String]
              xh = transpose $ alignRight $ map formatHints $ xhints b

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
