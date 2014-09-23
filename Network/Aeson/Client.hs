{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Network.Aeson.Client where

import Data.Aeson hiding (Result)
import Data.Attoparsec.ByteString (parseWith, IResult(..))
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromMaybe)
import Network.Http.Client
import OpenSSL (withOpenSSL)
import System.IO (hFlush, hPutStrLn, stderr)
import qualified System.IO.Streams as Streams

-- | Execute a GET agains the specified URI (e.g. `/v1`) using the
-- supplied parameters.
apiGet :: FromJSON a =>
  Maybe (B.ByteString, B.ByteString)
  -> B.ByteString
  -> B.ByteString
  -> [(B.ByteString, Maybe B.ByteString)] -> IO (Maybe a)
apiGet mbasic base uri parameters = withOpenSSL $ do
  let url = B.concat [uri, queryString parameters]
  q <- buildRequest $ do
    http GET url
    maybe (return ()) (uncurry setAuthorizationBasic) mbasic

  c <- establishConnection base
  -- debug q
  sendRequest c q emptyBody
  receiveResponse' c

-- | Execute a POST agains the specified URI (e.g. `/v1`) using the
-- supplied parameters.
apiPost :: FromJSON a =>
  Maybe (B.ByteString, B.ByteString)
  -> B.ByteString
  -> B.ByteString
  -> [(B.ByteString, Maybe B.ByteString)] -> LB.ByteString -> IO (Maybe a)
apiPost mbasic base uri parameters body = withOpenSSL $ do
  let url = B.concat [uri, queryString parameters]
  q <- buildRequest $ do
    http POST url
    maybe (return ()) (uncurry setAuthorizationBasic) mbasic
    setContentLength (fromIntegral $ LB.length body)
    setContentType "application/json"

  c <- establishConnection base
  -- debug q
  body' <- Streams.fromLazyByteString body
  sendRequest c q (inputStreamBody body')
  receiveResponse' c

-- | Execute a DELETE agains the specified URI (e.g. `/v1`) using the
-- supplied parameters.
apiDelete :: FromJSON a =>
  Maybe (B.ByteString, B.ByteString)
  -> B.ByteString
  -> B.ByteString
  -> [(B.ByteString, Maybe B.ByteString)] -> IO (Result a)
apiDelete mbasic base uri parameters = withOpenSSL $ do
  let url = B.concat [uri, queryString parameters]
  q <- buildRequest $ do
    http DELETE url
    maybe (return ()) (uncurry setAuthorizationBasic) mbasic

  c <- establishConnection base
  -- debug q
  sendRequest c q emptyBody
  -- TODO assert 204 for upcloud

  r <- receiveResponse c $ \p' i -> do
    case getStatusCode p' of
      204 -> return Ok
      _ -> do
        x <- Streams.read i
        let more = Streams.read i >>= return . fromMaybe ""
        p <- parseWith more json $ fromMaybe "" x
        case p of
          Done _ value -> do
            -- debug value
            case fromJSON value of
              Success value' -> do
                return $ Value value'
              _ -> return JsonFailure
          _ -> return ParseFailure
  closeConnection c
  return r

data Result a =
    Ok
  -- ^ Success and empty result
  | Value a
  -- ^ Succes and non-empty result
  | JsonFailure
  -- ^ Can't turn JSON into a proper result
  | ParseFailure
  -- ^ Can't parse JSON
  deriving Show

-- | Execute a PUT agains the specified URI using the
-- supplied parameters.
apiPut ::
  Maybe (B.ByteString, B.ByteString)
  -> B.ByteString
  -> B.ByteString
  -> [(B.ByteString, Maybe B.ByteString)]
  -> LB.ByteString
  -> IO ()
apiPut mbasic base uri parameters body = withOpenSSL $ do
  let url = B.concat [uri, queryString parameters]
  q <- buildRequest $ do
    http PUT url
    setContentLength (fromIntegral $ LB.length body)
    setContentType "application/json"
    maybe (return ()) (uncurry setAuthorizationBasic) mbasic

  c <- establishConnection base
  -- debug q
  body' <- Streams.fromLazyByteString body
  sendRequest c q (inputStreamBody body')
  -- TODO assert 204 for upcloud

-- | Execute a PATCH agains the specified URI using the
-- supplied parameters.
apiPatch ::
  Maybe (B.ByteString, B.ByteString)
  -> B.ByteString
  -> B.ByteString
  -> [(B.ByteString, Maybe B.ByteString)]
  -> LB.ByteString
  -> IO ()
apiPatch mbasic base uri parameters body = withOpenSSL $ do
  let url = B.concat [uri, queryString parameters]
  q <- buildRequest $ do
    http PATCH url
    setContentLength (fromIntegral $ LB.length body)
    setContentType "application/json"
    maybe (return ()) (uncurry setAuthorizationBasic) mbasic

  c <- establishConnection base
  -- debug q
  body' <- Streams.fromLazyByteString body
  sendRequest c q (inputStreamBody body')
  -- TODO assert 204 for upcloud

receiveResponse' :: FromJSON a => Connection -> IO (Maybe a)
receiveResponse' c = do
  r <- receiveResponse c $ \_ i -> do
    x <- Streams.read i
    let more = Streams.read i >>= return . fromMaybe ""
    p <- parseWith more json $ fromMaybe "" x
    case p of
      Done _ value -> do
        -- debug value
        case fromJSON value of
          Success value' -> do
            return $ Just value'
          _ -> return Nothing
      _ -> return Nothing
  closeConnection c
  return r

queryString :: [(B.ByteString, Maybe B.ByteString)] -> B.ByteString
queryString [] = ""
queryString xs = B.cons '?' . B.intercalate "&" . map f $ xs
  where f (a, Just b) = B.concat [a, "=", b]
        f (a, _) = a

debug :: Show a => a -> IO ()
debug s = do
  hPutStrLn stderr $ show s
  hFlush stderr
