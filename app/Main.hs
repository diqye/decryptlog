{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Data.ByteString.Base64(decodeBase64)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
-- import Codec.Crypto.AES
import Crypto.Hash.SHA1(hash)
import Data.String.Conversions(cs)
import Numeric
import qualified Crypto.Cipher.AES as CA
import System.Environment(getArgs)
import System.IO(withFile,IOMode(ReadMode,WriteMode),Handle,hIsEOF)
import Text.Regex.Posix((=~))
import Control.Monad
import System.Directory(getHomeDirectory)
import Data.String(fromString)
main :: IO ()
main = do
  args <- getArgs
  caseArg args

caseArg :: [String] -> IO ()
caseArg [filePath] = do
  let a = (filePath =~ ("[^/\\]+$"::String)) :: String
  let b = (a =~ ("[^\\.]*" :: String)) :: String
  caseArg [filePath,b]
caseArg [filePathOfArg,key] = do
  home <- getHomeDirectory
  let filePath = if head filePathOfArg == '~' then home <> tail filePathOfArg else filePathOfArg
  let hashOfkeyword = hash $ cs $ key
  let key = B.take 16 hashOfkeyword
  let iv = (B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1])
  withFile filePath ReadMode $ \readhandle-> withFile (filePath++"decrypt") WriteMode $ \writeHandle-> decryptlog readhandle writeHandle (key,iv)
caseArg _ = do
  putStrLn "Useage: decryptlog filepath [key]"
  putStrLn "For example:"
  putStrLn "decryptlog dafs.log matrx-sssuue_u" 
  putStrLn "decryptlog ./a/matrx-aDiu_ud.log"

decryptlog :: Handle -> Handle -> (B.ByteString,B.ByteString) -> IO ()
decryptlog readHandle writeHandle keyO =  do
  isEOF <- hIsEOF readHandle
  if isEOF then putStrLn "Successfully" else do
    a <- B.hGetLine readHandle
    let (time,encryptd) = T.breakOn "|+|" $ TE.decodeUtf8 $ a
    decrypted <- decryptLine (T.drop 3 encryptd) keyO
    C.hPutStrLn writeHandle $ cs time  <> decrypted
    decryptlog readHandle writeHandle keyO

trimStart :: T.Text -> T.Text 
trimStart text = if first == "\r" || first == "\n" then trimStart $ T.drop 1 text else text
  where first = T.take 1 text

decryptLine :: T.Text -> (B.ByteString,B.ByteString) -> IO B.ByteString
decryptLine encryptd (key,iv) | T.null encryptd = pure ""
                              | otherwise = do
  let base64' = T.reverse encryptd
  let base64 = trimStart base64'
  let base64Bytes = TE.encodeUtf8 base64 
  let (Right a)  = decodeBase64 $ base64Bytes
  pure $ CA.decryptCTR (CA.initAES key) iv a

-- main :: IO ()
-- main = do
--   let keyword = "matrx-DXo0U_08_c4"
--   let hashOfkeyword = hash keyword
--   let key = B.take 16 hashOfkeyword
--   let iv = (B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1])
--   let (Right a) = decodeBase64 $ B.reverse "=g/FHjhirPsRpGZX2K1KXCoiUPw5QSEaJMXM5yQM1vc2bIguZce+l+jWvkZZ8G6E4weN4ApYEEUHzR38nVBMgXR1eo/EbzrENj0BoijaonN2Dx0LB6SNqj05zo/AFzwBsZI8C82ZTWWjxd1lm18S+LGEGip5AhGT"
--   let r = CA.decryptCTR (CA.initAES key) iv a
--   putStrLn $ T.unpack $ (TE.decodeUtf8 r)
  -- let result = crypt' CTR  key (B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]) Decrypt a
  -- putStrLn $ show $ fmap (\a->showHex a "" )$ B.unpack result
  -- putStrLn $ cs $ result
  -- let keyword = "matrx-DXo0U_08_c4"
  -- let (Right bs) = decodeBase64  $ TE.encodeUtf8 $ T.reverse $ "n33ErLLqmtHqBpd28mBMJ9uReGNMUR5RgcJeDARTk4Wo/YUY5jggSo6GT++QHixTtnGKijJiGRAOXKCNxmE829sBIHSH/dZ9Yk0ZTWWjxd1lm18S+LGEGip5AhGT"
  -- putStrLn $ show $ fmap (\a->showHex a "" )$ B.unpack bs
  -- -- crypt' Decrypt 
  -- let hashOfkeyword = hash keyword
  -- let key = B.take 16 hashOfkeyword
  -- putStrLn $ show $ fmap (\a->showHex a "" )$ B.unpack key
  -- let result = crypt' CTR  key (B.pack [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]) Decrypt bs
  -- putStrLn $ show $ fmap (\a->showHex a "" )$ B.unpack result
  -- putStrLn $ T.unpack $ (TE.decodeUtf8 result)


