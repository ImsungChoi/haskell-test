{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Word8 (Word8)
import Data.List
import Data.Ord
import Data.Maybe

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.Bits as Bits

-- import qualified Data.ByteString.Lazy.Char8 as C


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
  case (mvs, mts) of
    (Just vs, Just ts) -> return $ Just $ filter(\ t -> elem (tid t) vs) ts
    (_, _) -> return Nothing
  -- case mvs of 
  --   Nothing -> return mts
  --   Just vs -> do
  --     case mts of
  --       Nothing -> return Nothing
  --       Just ts -> return $ Just $ filter(\t -> elem (tid t) vs) ts

test :: IO (Map String Integer)
test = do 
  t <- (getBadTs "victims.json" "transactions.json")
  return $ getFlow $ fromJust t

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow ts' = go ts' Map.empty
  where go :: [Transaction] -> Map String Integer -> Map String Integer
        go [] m     = m
        go (t:ts) m = let u1 = ((to t), (amount t))
                          u2 = ((from t), negate (amount t))
                          m' = Map.insertWith (+) (fst u1) (snd u1) m
                          m'' = Map.insertWith (+) (fst u2) (snd u2) m'
                      in go ts m''
        

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = fst $ maximumBy (comparing $ snd) (Map.toList m) 

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m ts = makeT ts (payers m) (payees m)  
  where payers :: Map String Integer -> [(String, Integer)]
        payers m' = reverse $ sort' $ filter (\ x -> (snd x) > 0) (Map.toList m')
        payees :: Map String Integer -> [(String, Integer)]
        payees m'' = sort' $ filter (\ x -> (snd x) < 0) (Map.toList m'')
        sort' :: [(String, Integer)] -> [(String, Integer)]
        sort' ls = sortBy (comparing $ snd) ls
        makeT :: [TId] -> [(String, Integer)] -> [(String, Integer)] -> [Transaction]
        makeT ts [] [] = []
        makeT (t:ts) (p1:p1s) (p2:p2s) = let am = if (snd p1) > (abs $ snd p2) then (abs $ snd p2) else snd p1 
                                             nt = Transaction {from = (fst p1), to = (fst p2), amount = am, tid = t}
                                             p1s' = if (snd p1) == am then p1s else (fst p1, (snd p1 - am)) : p1s
                                             p2s' = if (abs $ snd p2) == am then p2s else (fst p2, (snd p2 + am)) : p2s
                                         in (nt : makeT ts p1s' p2s')
-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON fp ts = BS.writeFile fp $ encode ts

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
