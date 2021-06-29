{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}
import Control.Arrow ((&&&))
import Control.Monad

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Word
import Data.Int
import Data.Char
import Data.Bits
import Data.Maybe
import Data.List
import Data.Function
import Numeric (showHex)

import System.Environment
import System.IO
import System.Exit

groupOn f = groupBy ((==) `on` f)

-- nop   | op | ...             |
-- Load  | op | ra  | rd  | ... |
-- store | op | rs  | ra  | ... |

-- loadI | op | rd  | ... | ... |
--       | hexValue             |

-- add   | op | rd  | rs1 | rs2 |
-- sub   | op | rd  | rs1 | rs2 |
-- mul   | op | rd  | rs1 | rs2 |
-- and   | op | rd  | rs1 | rs2 |
-- or    | op | rd  | rs1 | rs2 |
-- xor   | op | rd  | rs1 | rs2 |

-- bnz   | op | rc  | value     |
-- jump  | op | ...             |
--       | hexValue             |
-- ret   | op | ra  | ...       |
-- halt  | op | ...             |

type Memory = M.Map Word16 Word16
type Regs   = M.Map Char   Word16

load :: Word16 -> Memory -> Word16
load a = maybe 0 id . M.lookup a

load32 :: Word16 -> Memory -> Word32
load32 a m = (h `shiftL` 16) .|. l
  where
    h  = fromIntegral $ load a     m
    l  = fromIntegral $ load (a+1) m

loadHex :: Word16 -> Memory -> Word16
loadHex a = read . ("0x"++) . toBytes . load32 a

store :: Word16 -> Word16 -> Memory -> Memory
store k 0 = M.delete k
store k v = M.insert k v

readR :: Char -> Regs -> Word16
readR a rs
  | isDigit a = fromIntegral $ ord a - ord '0'
  | otherwise = maybe 0 id . M.lookup a $ rs

writeR :: Char -> Word16 -> Regs -> Regs
writeR k _
  | isDigit k = id
writeR k 0 = M.delete k
writeR k v = M.insert k v

data Machine = Machine
  { _effect :: Maybe (IO Machine)
  , _regs   :: Regs
  , _memory :: Memory
  }

toBytes :: Word32 -> String
toBytes d = [chr . fromIntegral $ 0xff .&. (d `shiftR` (8 * i)) | i <- [0..3]]

fromBytes :: String -> Word32
fromBytes bs = sum [c `shiftL` (8*i) | (i,c) <- zip [0..] . map (fromIntegral . ord) $ bs]

showInstruction :: Word32 -> String
showInstruction = toBytes

showInstruction' :: Word32 -> String
showInstruction' = go . toBytes
  where
    go ['N',_ ,_ ,_ ] = unwords ["nop"]

    go ['L',ra,rd,_ ] = unwords ["Load",  concat ["M[", [ra], "]"], "->", [rd]]
    go ['S',rs,ra,_ ] = unwords ["Store", concat [[rs], " -> ", "M[", [ra], "]"]]
    go ['I',rd,_ ,_ ] = unwords ["LoadI", concat [[rd], " <-"]]

    go ['+',ra,rb,rd] = unwords ["Add", [ra],"+",[rb], "->", [rd]]
    go ['-',ra,rb,rd] = unwords ["Sub", [ra],"-",[rb], "->", [rd]]
    go ['*',ra,rb,rd] = unwords ["Mul", [ra],"*",[rb], "->", [rd]]
    go ['/',ra,rb,rd] = unwords ["Div", [ra],"/",[rb], "->", [rd]]
    go ['%',ra,rb,rd] = unwords ["Mod", [ra],"%",[rb], "->", [rd]]

    go ['b',rc,h ,l ] = unwords ["Branch", [rc], concat ["=/= 0, -0x", [h,l]]]
    go ['B',rc,h ,l ] = unwords ["Branch", [rc], concat ["=/= 0,  0x", [h,l]]]
    go ['e',rc,h ,l ] = unwords ["Branch", [rc], concat ["== 0, -0x", [h,l]]]
    go ['E',rc,h ,l ] = unwords ["Branch", [rc], concat ["== 0,  0x", [h,l]]]
    go ['l',rc,h ,l ] = unwords ["Branch", [rc], concat ["< 0, -0x", [h,l]]]
    go ['<',rc,h ,l ] = unwords ["Branch", [rc], concat ["< 0,  0x", [h,l]]]
    go ['g',rc,h ,l ] = unwords ["Branch", [rc], concat ["> 0, -0x", [h,l]]]
    go ['>',rc,h ,l ] = unwords ["Branch", [rc], concat ["> 0,  0x", [h,l]]]

    go ['J',ra,_ ,_ ] = unwords ["Jump P+4 ->", [ra], "; P <- "]
    go ['R',ra,_ ,_ ] = unwords ["Return", [ra]]
    
    go ['!',ra,h ,l ] = unwords ["syscall", [ra], concat ["0x", [h,l]]]
    go ['H',_ ,_ ,_ ] = unwords ["Halt"]

    go hs
      | all (`elem` hexDigit) hs = "    0x" ++ hs
      | otherwise                = intercalate " " . map (lpad 2 '0' . flip showHex "" . ord) $ hs

    hexDigit = ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f']

readRegisters :: Word32 -> S.Set Char
readRegisters = S.fromList . go . toBytes
  where
    go [o,a,b,c]
      | o `elem` "S+-*/%"      = [a,b]
      | o `elem` "LR!bBeEl<g>" = [a]
      | otherwise              = []

writeRegisters :: Word32 -> S.Set Char
writeRegisters = S.fromList . go . toBytes
  where
    go [o,a,b,c]
      | o `elem` "+-*/%" = [c]
      | o `elem` "L"     = [b]
      | o `elem` "!IJ"   = [a]
      | otherwise        = []

registers :: Word32 -> S.Set Char
registers i = S.union (readRegisters i) (writeRegisters i)

stepPC :: Int -> Regs -> Regs
stepPC n regs = writeR 'P' (fromIntegral (fromIntegral (readR 'P' regs) + 2*n)) regs

stepInstruction :: Machine -> Either String Machine
stepInstruction m = go i
  where
    mem  = _memory m
    regs = _regs   m
    mem_  f m = m { _memory = f (_memory m) }
    regs_ f m = m { _regs = f (_regs m) }
    effect_ a m = m { _effect = Just a }

    r reg = readR reg regs
    rI reg = fromIntegral (fromIntegral (r reg) :: Int16)

    pc = readR 'P' regs
    i  = toBytes . load32 pc $ mem

    go ['N',_ ,_ ,_ ] = return . regs_ (stepPC 1) $ m

    go ['L',r->a,rd  ,_ ] = return . regs_ (stepPC 1 . writeR rd (load a mem)) $ m
    go ['S',r->s,r->a,_ ] = return . regs_ (stepPC 1) . mem_ (store a s) $ m
    go ['I',rd  ,_   ,_ ] = return . regs_ (stepPC 2 . writeR rd (loadHex (pc+2) mem)) $ m

    go ['+',ra,rb,rd] = arith (+) ra rb rd
    go ['-',ra,rb,rd] = arith (-) ra rb rd
    go ['*',ra,rb,rd] = arith (*) ra rb rd
    go ['/',ra,rb,rd] = arith div ra rb rd
    go ['%',ra,rb,rd] = arith mod ra rb rd

    go [c,rI->b, h,l]
      | isBranch c && (b `op` 0) = return . regs_ (stepPC off) $ m
      | isBranch c               = return . regs_ (stepPC 1)   $ m
      where
        isBranch = (`M.member` ops)
        off | isLower c = (read $ "-0x" ++ [h,l])
            | otherwise = (read $  "0x" ++ [h,l])
        op = fromJust $ M.lookup c ops -- protected by isBranch
        ops :: M.Map Char (Int -> Int -> Bool)
        ops = M.fromList (zip "bBeEl<g>" [(/=),(/=),(==),(==),(<),(<),(>),(>)])

    go ['J',ra  ,_ ,_ ] = return . regs_ (writeR ra (pc+4) . writeR 'P' (loadHex (pc+2) mem)) $ m
    go ['R',r->a,_ ,_ ] = return . regs_ (writeR 'P' a) $ m
    
    go ['H',_ ,_ ,_ ] = Left "Halt"
    go ['!',ra@(r->a),h,l] = return . effect_ (syscall (read $ "0x" ++ [h,l]) ra a (regs_ (stepPC 1) m)) $ regs_ (stepPC 1) m
    go inst           = Left . (showHex (readR 'P' regs) ":" ++) . (" Unknown instruction: "++)  . showInstruction . fromBytes $ inst

    arith op (r->a) (r->b) rd = return . regs_ (stepPC 1 . writeR rd (a `op` b)) $ m

readNumber :: (Integral i, Read i) => IO i
readNumber = do
    l <- getLine
    case reads l of
      [(n, [])] -> return n
      _         -> do
        hPutStrLn stderr "Invalid number"
        readNumber

syscall :: Int -> Char -> Word16 -> Machine -> IO Machine
syscall 1 r v m = do
    putStr . map (chr . fromIntegral) . takeWhile (/= 0) . map (flip load (_memory m)) $ [v..]
    hFlush stdout
    return (m { _effect = Nothing })
syscall 2 r v m = do
    n <- readNumber
    return (m { _effect = Nothing, _regs = writeR r n (_regs m) })
syscall n r v m = (putStrLn $ "Unknown syscall 0x" ++ showHex n ([' ',r])) >> return (m { _effect = Nothing })

mapChanges :: (Ord k, Num v, Eq v) => M.Map k v -> M.Map k v -> M.Map k v
mapChanges a b = M.fromList (old ++ [(k,v) | (k,v) <- bs
                                           , case M.lookup k a of
                                               Just v' -> v /= v'
                                               Nothing -> True])
  where
    bs = M.toAscList b
    old = map (,0) . M.keys $ M.difference a b

showReg :: Char -> Word16 -> String
showReg c w = c:':':' ':show (fromIntegral w :: Int16)

showMem :: Word16 -> Word16 -> String
showMem a w = showHex a $ ": " ++ show (fromIntegral w :: Int16)

showDiff :: Machine -> Machine -> IO ()
showDiff m m' = do
  let rs  = M.delete 'P' $ mapChanges (_regs m) (_regs m')
      ms  = mapChanges (_memory m) (_memory m')
      pc  = readR 'P' . _regs $ m
      pc' = readR 'P' . _regs $ m'

  putStr (showAddr pc ++ ": ")
  putStr (rpad 4 ' ' . showInstruction . flip load32 (_memory m) $ pc)
  putStr " -- "
  putStr (intercalate ", " . map (rpad  9 ' ' . uncurry showReg) . M.toAscList $ rs)
  putStr (rpad 11 ' ' "")
  putStr (intercalate ", " . map (rpad 16 ' ' . uncurry showMem) . M.toAscList $ ms)
  putStrLn ""
  
  

stepIO :: Bool -> Machine -> IO Machine
stepIO trace m = do
    case stepInstruction m of
        Left  e  -> when trace (putStrLn e) >> return m
        Right m' -> do
            when trace $ do
              showDiff m m'
            case _effect m' of
              Just act -> act >>= stepIO trace
              _        -> stepIO trace m'

signed :: Word16 -> Int16
signed v = fromIntegral v

split32 :: Word32 -> [Word16]
split32 v = map fromIntegral [v `shiftR` 16, v .&. 0xffff]

join16 h l = ((fromIntegral h) `shiftL` 16) .|. (fromIntegral l)

assemble :: [String] -> [Word16]
assemble = concatMap split32 . map fromBytes

disassemble :: [Word16] -> [String]
disassemble [] = []
disassemble (h:l:ws) = (showInstruction $ join16 h l) : disassemble ws

allRegisters :: [Word16] -> S.Set Char
allRegisters [] = S.empty
allRegisters (h:l:ws) = S.union (registers $ join16 h l) (allRegisters ws)

progA = assemble
    [ "Ia  "
    , "0100"
    , "Ib  "
    , "0001"
    , "+bcc"
    , "-aba"
    , "Ba02"
    , "H   "
    ]

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
lpad n c s = replicate (n - length s) c ++ s
rpad n c s = s ++ replicate (n - length s) c

data Page
  = Code { _offset :: Word16                  , _data :: [Word16] }
  | Data { _offset :: Word16, _viz :: [Word16], _data :: [Word16] }

isCodePage :: Page -> Bool
isCodePage Code{} = True
isCodePage _      = False

analyseFromFile :: FilePath -> IO [Page]
analyseFromFile f = do
    bs <- loadFromFile f
    print (allRegisters . concatMap _data . filter isCodePage $ bs)
    return bs

analyseRAW :: [Page] -> IO ()
analyseRAW ps = do
    let ps' = filter isCodePage ps
        is  = map (\p -> zip (zipWith (+) [0,2..] (cycle [_offset p])) (asWord32 $ _data p)) ps'
        rs  = concatMap raw is
    forM_ rs $ \((a,i),(b,j)) -> do
      putStrLn $ showHex a ":" ++ showInstruction i
      putStrLn $ showHex b ":" ++ showInstruction j
      putStrLn "---"
  where
    asWord32 [] = []
    asWord32 (h:l:vs) = join16 h l : asWord32 vs
    raw = filter (uncurry isRaw) . (zip <*> tail)
    isRaw (a,i) (b,j) = (> 0) . S.size $ S.intersection (readRegisters j) (writeRegisters i)

loadFromFile :: FilePath -> IO [Page]
loadFromFile f = go . filter (not . isComment) . map trim . lines <$> readFile f
  where
    go [] = []
    go (l:ls)
      | isData l  = goData (map read . words . drop 5 . uncomment $ l) ls
      | isCode l  = goCode (read . head . words . drop 5 . uncomment $ l) ls
      | otherwise = goCode 0 (l:ls)

    goCode o ls = let (bs, ls') = chunk ls
                      c = Code o . assemble . map (take 4 . (++ repeat ' ')) $ bs
                  in  c : go ls'
    goData (o:vs) ls = let (bs, ls') = chunk ls
                           c = Data o vs . map read . concatMap words . map uncomment $ bs
                       in  c : go ls'

    isData = ("data:" `isPrefixOf`)
    isCode = ("code:" `isPrefixOf`)
    chunk = break (\l -> isData l || isCode l)
    uncomment = trim . fst . break (== '#')

isComment :: String -> Bool
isComment ""      = True
isComment ('#':_) = True
isComment _       = False

runFromFile :: Bool -> FilePath -> IO Machine
runFromFile trace f = do
     ps <- loadFromFile f
     let m = mkMachine ps
     stepIO trace m

mkMachine :: [Page] -> Machine
mkMachine ps = Machine Nothing M.empty . M.fromList $ concat [zip ks vs | (o,vs) <- bs, let ks = [o..]]
  where
    bs = map (\p -> (_offset p, _data p)) ps

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n as = take n as : chunksOf n (drop n as)

bitmapIO :: Char -> Char -> Word16 -> Machine -> IO ()
bitmapIO w h o m = mapM_ putStrLn . map (map f) . chunksOf (fromIntegral w') . map (`load` (_memory m)) $ [o..o+w'*h'-1]
  where
    f 0 = ' '
    f _ = 'X'
    w' = fromIntegral $ readR 'w' (_regs m) 
    h' = fromIntegral $ readR 'h' (_regs m)

showAddr n = reverse . take 4 . (++ repeat '0') . reverse $ showHex n ""

main = do
  as <- getArgs
  case as of 
    [mode,f] -> do
      case map toLower mode of
        "trace" -> runFromFile True f >> return ()
        "mem"   -> do
          ps <- loadFromFile f
          let m = mkMachine ps
          m' <- stepIO False m
          forM_ (filter (not . isCodePage) ps) $ \p ->
            case p of
              Data o [v] _ -> do
                putStr (showAddr o ++ ": ")
                print $ map (flip load (_memory m')) [o..o+v-1]
              _memory      -> return ()
        "io" -> runFromFile False f >> return ()
        "haz" -> do
          bs <- loadFromFile f
          analyseRAW bs
          return ()
    _ -> do
      hPutStrLn stderr $ unlines $
        [ "Usage: R-ASCII [mode] [file.ram]"
        , "Modes:"
        , "\ttrace -- Output program trace as program executes."
        , "\tmem   -- Dump memory after program execution."
        , "\tio    -- Run program with IO effects."
        , "\thaz   -- Analyze program for read-after-write hazards."
        ]
      exitWith (ExitFailure 1)