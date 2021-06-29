-- Adventure compiler
import Control.Arrow (second)
import Data.Char
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment
import System.IO
import System.Exit

import Paths_R_ASCII (getDataFileName)

import Numeric

data Node a = Node
    { _name :: String
    , _desc :: String
    , _opts :: [(a, String)]
    } deriving (Show, Eq)

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

parseNode :: [String] -> (Node String, [String])
parseNode (n:"":as) = let (ds, bs) = second tail . break (=="") $ as
                          (os, cs) = second tail . break (=="") $ bs
                      in  ( Node (takeWhile (/= ':') n)
                                 (unlines ds)
                                 (map (second (trim . tail) . break (== ')')) os)
                          , dropWhile (== "") cs
                          )
parseNode ls = error ("Failed to parse from:\n " ++ unlines (take 5 ls))

parse :: [String] -> [Node String]
parse ls = case parseNode ls of
             (n, []) -> [n]
             (n, ls') -> n : parse ls'

build :: [Node String] -> M.Map Int (Node Int)
build ns = M.map stitch m
  where
    m = M.fromList $ zip [0..] ns
    names = M.fromList $ zip (map _name ns) [0..]

    stitch n = n { _opts = map thread (_opts n) }

    thread (n,opt) = case M.lookup n names of
                        Just i  -> ( i,opt)
                        Nothing -> (-1,opt)

mkData :: M.Map Int (Node Int) -> [String]
mkData m = concat
    [ mkRecord d [t,r,w]
    , concatMap (uncurry mkRecord) [(g+i*r, length os : map fst os) | (i,os) <- gs]
    , concatMap (uncurry mkRecord) [(t+i*w, map ord s)  | (i, s) <- ts]
    ]
  where
    -- d is data offset
    -- g is graph offset
    -- t is text offset
    -- r is graph record size
    -- w is text record size
    -- s is state
    d = 0x1000
    g = d + 3
    t = g + r * length ns

    r = succ . maximum . map (length . snd) $ gs
    w = succ . maximum . map (length . snd) $ ts

    ns = M.toAscList m

    gs = map (second _opts) ns
    ts = map (second mkText) ns

mkText :: Node Int -> String
mkText n = "\n" ++ _desc n ++ "\n" ++ unlines [show i ++ ") " ++ o | (i,(_,o)) <- zip [0..] $ _opts n] ++ "\n> "

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n vs = take n vs : chunksOf n (drop n vs)

mkRecord :: Int -> [Int] -> [String]
mkRecord o vs = ("data: 0x" ++ showHex o "") : (map unwords . chunksOf 16 . map show $ vs)

main = do
    as <- getArgs
    case as of
      [a,"-o",o] -> do
        ls <- map trim . lines <$> readFile a
        let ns = parse ls
            m  = build ns
        headerPath <- getDataFileName "data/adventure-header.ram"
        header <- readFile headerPath
        writeFile o (header ++ "\n\n" ++ unlines (mkData m))
      _ -> do
        hPutStrLn stderr "Usage: adventure-compiler [text-graph.txt] -o [output.ram]"
        exitWith (ExitFailure 1)

