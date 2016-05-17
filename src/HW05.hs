{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Word8 (Word8)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.Bits as Bits

import qualified Data.ByteString.Lazy.Char8 as C


import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret f1 f2 = do
  rf1 <- BS.readFile f1
  rf2 <- BS.readFile f2
  return (BS.filter (/=0) (BS.pack $ BS.zipWith (Bits.xor) rf1 rf2))

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey k f = do
  rf <- BS.readFile $ f ++ ".enc"
  BS.writeFile f $ BS.pack $ go k rf
    where go :: ByteString -> ByteString -> [Word8]
          go k' b = zipWith (Bits.xor) (cycle $ BS.unpack k') (BS.unpack b)

-- Exercise 3 ----------------------------------------- 

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile f = do
  rf <- BS.readFile f
  return $ decode rf

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vp tp = do
  mvs <- parseFile vp :: IO (Maybe [TId])
  mts <- parseFile tp
  case mvs of 
    Nothing -> return mts
    Just vs -> do
      case mts of
        Nothing -> return Nothing
        Just ts -> return $ Just $ filter(\t -> elem (tid t) vs) ts


-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts = go Map.empty ts
  where go :: Map String Integer -> [Transaction] -> Map String Integer
        go m []       = m
        go m (t:ts')  = let mv = Map.lookup (to t) m 
                        in case mv of
                          Nothing -> go (Map.insert (to t) (amount t) m) ts'
                          Just v  -> let f _ = Just (v + (amount t))
                                     in go (Map.update f (to t) m) ts'

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = go (Map.elemAt 0 m) (tail (Map.toList m))
  where go :: (String, Integer) -> [(String, Integer)] -> String
        go c []     = fst c
        go c (p:ps) = if (snd c) < (snd p) 
                      then go p ps
                      else go c ps

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON fp ts = do
  BS.writeFile fp $ encode ts

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main' :: IO ()
main' = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
