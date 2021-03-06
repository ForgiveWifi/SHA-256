{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Sha256 (sha256) where

import Data.List (transpose, replicate)
import Data.Char (ord,chr)

type Bit = Bool
type Byte = [Bit]
type Constant = Byte

type Block = [Byte]
type Hash = [Byte]


sha256 :: String -> String
sha256 = hashToHex . compressionMessage . messageSchedule . paddingString


--Shift and Rotate 
shR :: Int -> Byte -> Byte
shR 0 byte = byte
shR n byte = shR (n-1) $ False : init byte

rotR :: Int -> Byte -> Byte
rotR 0 byte = byte
rotR n byte = rotR (n-1) $ last byte : init byte



--Sigma functions
uSigma0 :: Byte -> Byte
uSigma0 byte = xor $ rotR 2 byte : rotR 13 byte : [rotR 22 byte]

uSigma1 :: Byte -> Byte
uSigma1 byte = xor $ rotR 6 byte : rotR 11 byte : [rotR 25 byte]

sigma0 :: Byte -> Byte
sigma0 byte = xor $ rotR 7 byte : rotR 18 byte : [shR 3 byte]

sigma1 :: Byte -> Byte
sigma1 byte = xor $ rotR 17 byte : rotR 19 byte : [shR 10 byte]



--Xor, Ch, Maj, Addition
xor :: [Byte] -> Byte
xor = map xorf . transpose
  where
    xorf :: Byte -> Bit
    xorf byte
      | and byte = True
      | length (filter (==True) byte) == 1 = True
      | otherwise = False

choices :: [Byte] -> Byte
choices = map choicef . transpose
  where
    choicef :: Byte -> Bit
    choicef [x,y,z]
      | x = y
      | otherwise = z

majority :: [Byte] -> Byte
majority = map majorityf . transpose
  where
    majorityf :: Byte -> Bit
    majorityf [x,y,z]
      | x == y = x
      | y == z = y
      | otherwise = z

addition :: [Byte] -> Byte
addition byte = byteLength 32 . intToByte $ sumBinary `mod` (2 ^ 32)
  where
    sumBinary = sum $ map byteToInt byte



--Padding
paddingString :: String -> [Byte]
paddingString = paddingMessage . byteSplit 512 . stringToByte

paddingMessage :: [Byte] -> [Byte]
paddingMessage msg
    | length lastBlock <= 447 = init msg ++ [lastBlock ++ [True] ++ replicate n False ++ msgLength]
    | otherwise               = init msg ++ byteSplit 512 (lastBlock ++ [True] ++ replicate (512 - negate n) False ++ msgLength)
  where
    n = 448 - (length lastBlock + 1)
    lastBlock = last msg
    msgLength = messageLength msg

messageLength :: [Byte] -> Byte
messageLength = byteLength 64 . intToByte . sum . map length



--Message Schedule
messageSchedule :: [Byte] -> [Block]
messageSchedule = map messageBlock

messageBlock :: Byte -> [Byte]
messageBlock = extendSchedule . byteSplit 32

extendSchedule :: [Byte] -> [Byte]
extendSchedule block
    | length block < 64 = extendSchedule (block ++ [mkSchedule block])
    | otherwise = block

mkSchedule :: Block -> Byte
mkSchedule schedule = addition $ sigma1 (index $ i - 2) : index (i - 7) : sigma0 (index $ i - 15) : [index (i - 16)]
  where
    i = length schedule
    index = (schedule !!)



--Compression 
compressionMessage :: [Block] -> Hash
compressionMessage (x:xs) = compression xs $ initialCompression x

initialCompression :: Block -> Hash
initialCompression x = compressionBlock x constants initialHashValues initialHashValues

compression :: [Block] -> Hash -> Hash
compression [] values = values
compression (x:xs) values = compression xs (compressionBlock x constants values values)

compressionBlock :: Block -> [Constant] -> Hash -> Hash -> Hash
compressionBlock [] [] values ivalues = addInitialValue [values,ivalues]
compressionBlock (x:xs) (y:ys) values ivalues = compressionBlock xs ys (compress x y values) ivalues

addInitialValue :: [Hash] -> Hash
addInitialValue = map addition . transpose

compress :: Byte -> Constant -> Hash -> Hash
compress word k values = addition [t1,t2] : a : b : c : addition [d,t1] : e : f : [g]
  where
    t1 = addition $ h : uSigma1 e : choices [e,f,g] : k : [word]
    t2 = addition $ uSigma0 a : [majority [a,b,c]]
    a = index 0
    b = index 1
    c = index 2
    d = index 3
    e = index 4
    f = index 5
    g = index 6
    h = index 7
    index = (values !!)



--Primes Numbers
primes :: [Int]
primes = [ x | x <- [1..], isPrime x ]
  where
    isPrime :: Int -> Bool
    isPrime n = length [ x | x <- [2..n], n `mod` x == 0] == 1

first8Primes :: [Int]
first8Primes = take 8 primes

first64Primes :: [Int]
first64Primes = take 64 primes



--Square and Cube Roots
primeSqrtRoot :: Int -> Byte
primeSqrtRoot = hexToByte . intToHex . truncate . two32 . takeDecimal . sqrt . fromIntegral

primeCubeRoot :: Int -> Byte
primeCubeRoot = hexToByte . intToHex . truncate . two32 . takeDecimal . cubeRoot . fromIntegral
  where
    cubeRoot x = x ** (1/3)

takeDecimal :: Double -> Double
takeDecimal n
  | n > 1 = takeDecimal (n - 1.0)
  | otherwise = n

two32 :: Double -> Double
two32 n = n * 2 ^ 32



--Constants and Hash Variables 
constants :: [Constant]
constants = map primeCubeRoot first64Primes

initialHashValues :: [Constant]
initialHashValues = map primeSqrtRoot first8Primes



--Converstions 
intToByte :: Int -> Byte
intToByte 0 = []
intToByte n = intToByte (div n 2) ++ [binary $ mod n 2]
  where 
    binary n = case n of 
      1 -> True
      0 -> False

byteToInt :: Byte -> Int
byteToInt [] = 0
byteToInt (x:xs) = fromEnum x * (2 ^ n) + byteToInt xs
  where
    n = length (x:xs) - 1


intToHex :: Int -> String
intToHex 0 = []
intToHex n = intToHex (n `div` 16) ++ [findHexValue $ n `mod` 16 ]

hexToInt :: String -> Int
hexToInt [] = 0
hexToInt (x:xs) = findHexKey x * 16 ^ n + hexToInt xs
  where
    n = length (x:xs) - 1


hexToByte :: String -> Byte
hexToByte = concatMap (byteLength 4 . intToByte . findHexKey)

byteToHex :: Byte -> String
byteToHex = intToHex . byteToInt


hashToHex :: Hash -> String
hashToHex = concatMap hexHash
  where
    hexHash :: Byte -> String
    hexHash = concatMap byteToHex . byteSplit 4

stringToByte :: String -> Byte
stringToByte = concatMap (byteLength 8 . intToByte . ord)



--Length of Bytes 
byteLength :: Int -> Byte -> Byte
byteLength n byte
  | length byte == n = byte
  | otherwise = byteLength n (False : byte)

byteSplit :: Int -> Byte -> [Byte]
byteSplit n byte = case splitAt n byte of
    (x,[]) -> [x]
    (x,y) -> x : byteSplit n y



--Hex Map 
hexMap :: [(Char,Int)]
hexMap = zip hexKey [0..15]
  where
    hexKey = ['0'..'9'] ++ ['a'..'f']

findHexKey :: Char -> Int
findHexKey = findKey hexMap

findHexValue :: Int -> Char
findHexValue = findValue hexMap



--Lookup 
findKey :: Eq k => [(k,v)] -> k -> v
findKey (x:xs) k
  | k == fst x = snd x
  | otherwise = findKey xs k

findValue :: Eq v => [(k,v)] -> v -> k
findValue (x:xs) v
  | v == snd x = fst x
  | otherwise = findValue xs v










