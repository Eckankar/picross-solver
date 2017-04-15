{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Monad (unless)
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

    showList fs s = s ++ "[" ++ concatMap show fs ++ "]"

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

-- What are all possible ways we can place the given hints in a row of size n?
allPossiblePlacements :: Hints -> Int -> [[Field]]
allPossiblePlacements [] n = [ replicate n Empty ]
allPossiblePlacements (h:hs) n | h > n             = []
                               | h == n && null hs = [ replicate h Marked ]
                               | otherwise         =  map (Empty :) (allPossiblePlacements (h:hs) (n-1)) ++
                                                      map ((replicate h Marked ++ [Empty]) ++) (allPossiblePlacements hs (n-h-1))

-- Are two rows compatible with each other?
isCompatiblePlacement :: [Field] -> [Field] -> Bool
isCompatiblePlacement a b = and $ zipWith (\x y -> x == y || x == Unknown || y == Unknown) a b

-- Determine which spots we're sure of.
determineCommonalities :: [[Field]] -> [Field]
determineCommonalities fs = map commonality $ transpose fs
    where commonality xs | all (== Marked) xs = Marked
                         | all (== Empty)  xs = Empty
                         | otherwise          = Unknown

-- Put down the fields that we know for sure based on the currently marked fields and hints.
placeGuaranteedFields :: Hints -> [Field] -> Int -> [Field]
placeGuaranteedFields hs r n = determineCommonalities $ filter (isCompatiblePlacement r) $ allPossiblePlacements hs n

-- Go through all rows, and mark any fields we're sure of.
placeGuaranteedRows :: Board -> Board
placeGuaranteedRows b = b { fields = zipWith3 placeGuaranteedFields (yhints b) (fields b) (repeat $ ydim b) }

-- Go through all columns, and mark any fields we're sure of.
placeGuaranteedCols :: Board -> Board
placeGuaranteedCols b = b { fields = transpose $ zipWith3 placeGuaranteedFields (xhints b) (transpose $ fields b) (repeat $ xdim b) }

solve :: Board -> IO Board
solve b = do let b' = placeGuaranteedRows b
             unless (b == b') $ traceShowM b
             let b'' = placeGuaranteedCols b'
             unless (b' == b'') $ traceShowM b'
             if b /= b''
             then solve b''
             else return b

isSolved :: Board -> Bool
isSolved = not . any (elem Unknown) . fields

processFile :: FilePath -> IO ()
processFile fileName = do b <- loadFile fileName
                          s <- solve b
                          traceShowM s
                          unless (isSolved s) $ putStrLn "Board could not be solved using known methods."
                          return ()

main :: IO ()
main = do files <- getArgs
          mapM_ processFile files
