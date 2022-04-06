import Control.Applicative ((<$>), Applicative(..),
                            Alternative((<|>)))
import Control.Monad (unless)
import Data.Char (isDigit)
import Data.List (transpose)
import qualified Data.Text as T
import Debug.Trace (traceShowM)
import System.Environment (getArgs)
import Text.ParserCombinators.ReadP

-- Picross solver implemented in Haskell
-- Sell examples directory for examples of input

data MegaHint = First Int | Second Int | Both Int
    deriving (Show,Eq)
data Hints = SingleHints [Int] | MegaHints [MegaHint]
    deriving (Show,Eq)
data Field = Unknown | Empty | Marked
    deriving (Eq)

instance Show Field where
    show Unknown = " "
    show Empty   = "."
    show Marked  = "#"

    showList fs s = "[" ++ concatMap show fs ++ "]" ++ s

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
        where formatHints :: Hints -> [String]
              formatHints (SingleHints hs) = [ unwords $ map show hs ]
              formatHints (MegaHints hs) = map init [ fa, sa ]
                where (fa, sa) = alignAndJoin $ foldl formatMegaHint ("", "", "", "") hs

              alignAndJoin (fh, sh, fa, sa) = (fa ++ fh', sa ++ sh')
                where [fh', sh'] = alignRight [fh, sh]

              formatMegaHint (fh, sh, fa, sa) (First n) =
                (fh ++ show n ++ " ", sh, fa, sa)
              formatMegaHint (fh, sh, fa, sa) (Second n) =
                (fh, sh ++ show n ++ " ", fa, sa)
              formatMegaHint (fh, sh, fa, sa) (Both n) =
                ("", "",
                 fa' ++ "┌" ++ show n ++ "┐ ",
                 sa' ++ "└" ++ map (const ' ') (show n) ++ "┘ ")
                where (fa', sa') = alignAndJoin (fh, sh, fa, sa)

              padLeft n s = replicate d ' ' ++ s
                where d = n - length s

              alignRight :: [String] -> [String]
              alignRight ss = map (padLeft ml) ss
                where ml = maximum $ map length ss

              blines = map (concatMap show) $ fields b
              flines = ["┌" ++ replicate (xdim b) '─' ++ "┐"] ++
                       map (\l -> "│" ++ l ++ "│") blines ++
                       ["└" ++ replicate (xdim b) '─' ++ "┘"]

              swapCorners '└' = '┐'
              swapCorners '┐' = '└'
              swapCorners c = c

              yh :: [String]
              yh = alignRight $ concatMap formatHints $ yhints b

              xh :: [String]
              xh = map (map swapCorners) $ transpose $
                    alignRight $ concatMap formatHints $ xhints b

parseInt :: ReadP Int
parseInt = do
    sign <- option '0' (char '-')
    digits <- many (satisfy isDigit)
    return $ read $ sign : digits

parseSingleHints :: ReadP Hints
parseSingleHints = SingleHints <$> sepBy parseInt (char ' ')

parseMegaHint :: ReadP MegaHint
parseMegaHint = parseH 'd' Both <|> parseH 'f' First <|> parseH 's' Second
    where parseH c f = f <$> (char c >> parseInt)

parseMegaHints :: ReadP Hints
parseMegaHints = MegaHints <$> sepBy parseMegaHint (char ' ')

parseHints :: ReadP [Hints]
parseHints = sepBy (parseSingleHints <|> parseMegaHints) (char '\n')

parseFile :: ReadP ([Hints], [Hints])
parseFile = do
    xhints <- parseHints
    skipSpaces
    string "---\n"
    yhints <- parseHints
    skipSpaces
    eof
    return (xhints, yhints)

runParser p = fst . head . filter (\(_,"") -> True) . readP_to_S p

getDimension = sum . map dim
    where dim (SingleHints _) = 1
          dim (MegaHints _) = 2

loadFile :: FilePath -> IO Board
loadFile fileName = do
    f <- readFile fileName
    let (xhs, yhs) = runParser parseFile f
    let (xd, yd) = (getDimension xhs, getDimension yhs)
    let fs = replicate yd $ replicate xd Unknown
    return Board { xhints = xhs, yhints = yhs,
                   fields = fs, xdim = xd, ydim = yd }

-- What are all possible ways we can place the given hints in a row of size n?
allPossiblePlacements :: [Int] -> Int -> [[Field]]
allPossiblePlacements [] n = [ replicate n Empty ]
allPossiblePlacements (h:hs) n
    | h > n             = []
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

data GroupedMH = GBoth Int | GSplit [Int] [Int]
    deriving (Show)

allPossibleMegaPlacements :: [MegaHint] -> Int -> [[[Field]]]
allPossibleMegaPlacements hs n = allPlacements' ghs n n [] []
    where -- grouped hints in reverse order
          ghs = foldl groupHints [] hs

          groupHints acc (Both n) = GBoth n : acc
          groupHints (GSplit fs ss : accs) (First n)  = GSplit (n:fs) ss : accs
          groupHints (GSplit fs ss : accs) (Second n) = GSplit fs (n:ss) : accs
          groupHints acc (First n)  = GSplit [n] [] : acc
          groupHints acc (Second n) = GSplit [] [n] : acc

          allPlacements' :: [GroupedMH] -> Int -> Int -> [Field] -> [Field] -> [[[Field]]]
          allPlacements' _ n m fr sr | n < 0 || m < 0 = []
          allPlacements' [] n m fr sr =
            [ [ replicate n Empty ++ fr, replicate m Empty ++ sr ] ]
          allPlacements' (GSplit [] [] : ghs) n m fr sr =
            allPlacements' ghs n m fr sr
          allPlacements' (GSplit fs ss : ghs) n m fr sr =
            (case fs of (f:fs') ->
                -- Try to place in the first row
                            let n' = n - f - 1
                                dm = m - n' - 1
                                fr' = replicate f Marked ++ fr
                                (n'', fr'') =
                                    if n' >= 0 then (n', Empty : fr')
                                               else (n'+1, fr')
                            in
                            allPlacements' (GSplit fs' ss : ghs) n'' (n'+1)
                                           fr''
                                           (replicate dm Empty ++ sr)
                        [] -> []
            ) ++
            (case ss of (s:ss') ->
                -- Try to place in the first row
                            let m' = m - s - 1
                                dn = n - m' - 1
                                sr' = replicate s Marked ++ sr
                                (m'', sr'') =
                                    if m' >= 0 then (m', Empty : sr')
                                               else (m'+1, sr')
                            in
                            allPlacements' (GSplit fs ss' : ghs) (m'+1) m''
                                           (replicate dn Empty ++ fr)
                                           sr''
                        [] -> []
            ) ++
                allPlacements' (GSplit fs ss : ghs) (n-1) (m-1)
                               (Empty : fr) (Empty : sr)
          allPlacements' (GBoth b : ghs) n m fr sr =
            placeBoth b n m fr sr True True False
                where placeBoth 0 _ _ _ _ _ _ False = []
                      placeBoth r n' m' _ _ _ _ _ | r < 0 || n' < 0 || m' < 0 || r > n' + m' = []
                      placeBoth 0 n' m' fr' sr' fl sl True =
                        allPlacements' ghs n'' m'' fr'' sr''
                        where (fr'', n'') = if fl && n' > 0 then (Empty : fr', n' - 1)
                                                  else (fr', n')
                              (sr'', m'') = if sl && m' > 0 then (Empty : sr', m' - 1)
                                                  else (sr', m')
                      placeBoth r n' m' fr' sr' fl sl sb =
                        let options = concat
                                        [
                                          -- only allow placing none in a column if we haven't
                                          -- started yet
                                          if r == b then [(Empty, Empty)] else [],
                                          -- only allow picking a single column if we're
                                          -- continuing on that column, and we're not
                                          -- starting with it empty already
                                          if (fl || r == b) && n' >= m' then [(Marked, Empty)] else [],
                                          if (sl || r == b) && m' >= n' then [(Empty, Marked)] else [],
                                          -- only allow placing in both if we haven't got an
                                          -- empty blocking
                                          if n' == m' then [(Marked, Marked)] else []
                                        ]
                        in concatMap (placeBoth' r n' m' fr' sr' fl sl sb) options

                      placeBoth' r n' m' fr' sr' fl sl sb (fc, sc) =
                        placeBoth r' nm'' nm'' fr'' sr'' fl' sl' sb'
                        where r' = r - (if fl' then 1 else 0)
                                     - (if sl' then 1 else 0)
                              nm'' = max n' m' - 1
                              fr'' = if n' >= m' then fc:fr' else fr'
                              sr'' = if m' >= n' then sc:sr' else sr'
                              fl' = fc == Marked
                              sl' = sc == Marked
                              sb' = sb || (fc, sc) == (Marked, Marked)

-- Put down the fields that we know for sure based on the currently marked fields and hints.
placeGuaranteedFields :: [Hints] -> [[Field]] -> Int -> [[Field]]
placeGuaranteedFields (SingleHints h : hs) (r:rs) n =
    (determineCommonalities $ filter (isCompatiblePlacement r) $ allPossiblePlacements h n) :
    placeGuaranteedFields hs rs n
placeGuaranteedFields (MegaHints h : hs) (r1:r2:rs) n =
    c0 : c1 : placeGuaranteedFields hs rs n
    where allPlacements = allPossibleMegaPlacements h n
          c0 = determineCommonalities (map (!! 0) compatiblePlacements)
          c1 = determineCommonalities (map (!! 1) compatiblePlacements)
          compatiblePlacements = filter (\[r1', r2'] -> isCompatiblePlacement r1 r1' && isCompatiblePlacement r2 r2') allPlacements
placeGuaranteedFields [] _ _ = []

-- Go through all rows, and mark any fields we're sure of.
placeGuaranteedRows :: Board -> Board
placeGuaranteedRows b = b { fields = placeGuaranteedFields (yhints b) (fields b) (xdim b) }

-- Go through all columns, and mark any fields we're sure of.
placeGuaranteedCols :: Board -> Board
placeGuaranteedCols b = b { fields = transpose $ placeGuaranteedFields (xhints b) (transpose $ fields b) (ydim b) }

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
processFile fileName = do
    b <- loadFile fileName
    traceShowM b
    s <- solve b
    traceShowM s
    unless (isSolved s) $ putStrLn "Board could not be solved using known methods."

main :: IO ()
main = do files <- getArgs
          mapM_ processFile files
