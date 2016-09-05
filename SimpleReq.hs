module SimpleReq where

import Control.Lens ((^.))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.CaseInsensitive (original)
import Data.Map (fromList)
import Control.Monad.Except (throwError, liftIO)
import Network.Wreq

import qualified Zepto.Types as T

exports :: [(String, [T.LispVal] -> T.IOThrowsError T.LispVal, String)]
exports = [("simplereq:get", makeReq get, makeReqDoc "get"),
           --("simplereq:post", makeBReq post, makeReqDoc "post"),
           --("simplereq:put", makeBReq put, makeReqDoc "put"),
           ("simplereq:delete", makeReq delete, makeReqDoc "delete")]

makeReqDoc :: String -> String
makeReqDoc method =
  intersperse ["takes a string <par>url</par> and performs a " ++ method ++ " request to that URL.",
               "",
               "params:",
               "- url: the URL to access",
               "complexity: dependent on the complexity of the response",
               "returns: a string representation of the response"]
    where intersperse [] = ""
          intersperse (x:xs) = x ++ "\n" ++ intersperse xs

treatResponse :: Network.Wreq.Response ByteString -> T.LispVal
treatResponse r =
  T.HashMap $ fromList
        [(T.Atom ":status", T.fromSimple $ T.Number (T.NumS (r ^. responseStatus . statusCode))),
         (T.Atom ":body", T.fromSimple $ T.String $ show (r ^. responseBody)),
         (T.Atom ":headers", T.HashMap $ fromList $ treatHeaders)]
    where treatHeaders = map treatHeader (r ^. responseHeaders)
          treatHeader (header, val) = (T.String $ unpack $ original header,
                                       T.fromSimple $ T.String $ unpack val)

makeBReq :: (String -> IO (Response ByteString)) -> [T.LispVal] -> T.IOThrowsError T.LispVal
makeBReq fun [T.SimpleVal (T.String url)] = do
  res <- liftIO $ fun url
  return $ treatResponse res
makeBReq _ [x, _] = throwError $ T.TypeMismatch "string" x
makeBReq _ [_, x] = throwError $ T.TypeMismatch "hashmap" x
makeBReq _ x = throwError $ T.NumArgs 1 x

makeReq :: (String -> IO (Response ByteString)) -> [T.LispVal] -> T.IOThrowsError T.LispVal
makeReq fun [T.SimpleVal (T.String url)] = do
  res <- liftIO $ fun url
  return $ treatResponse res
makeReq _ [x] = throwError $ T.TypeMismatch "string" x
makeReq _ x = throwError $ T.NumArgs 1 x
