module SimpleReq where

import Control.Lens ((^.), set)
import Control.Monad.Except (throwError, liftIO)
import Control.Exception (catch)
import qualified Data.ByteString (ByteString)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.CaseInsensitive (original)
import Data.Map (fromList)
import Network.Wreq

import qualified Zepto.Types as T

exports :: [(String, [T.LispVal] -> T.IOThrowsError T.LispVal, String)]
exports = [("simplereq:get", makeReq $ wrap getWith, makeReqDoc "get"),
           ("simplereq:post", makeBReq $ wrap postWith, makeBReqDoc "post"),
           ("simplereq:put", makeBReq $ wrap putWith, makeBReqDoc "put"),
           ("simplereq:delete", makeReq $ wrap deleteWith, makeReqDoc "delete"),
           ("simplereq:request", makeGenericReq, genericReqDoc)]

wrap f = f (set checkStatus (Just $ \_ _ _ -> Nothing) defaults)

intersperse :: [String] -> String
intersperse [] = ""
intersperse (x:xs) = x ++ "\n" ++ intersperse xs

makeReqDoc :: String -> String
makeReqDoc method =
  intersperse ["takes a string <par>url</par> and performs a " ++ method ++ " request to that URL.",
               "",
               "params:",
               "- url: the URL to access",
               "complexity: dependent on the complexity of the response",
               "returns: a hashmap with the keys <zepto>:status</zepto>, <zepto>:body</zepto> and <zepto>headers</zepto>"]

makeBReqDoc :: String -> String
makeBReqDoc method =
  intersperse ["takes a string <par>url</par> and performs a " ++ method ++ " request to that URL",
               "using the body specified in <par>body</par>.",
               "",
               "params:",
               "- url: the URL to access",
               "- body: the body to send",
               "complexity: dependent on the complexity of the response",
               "returns: a hashmap with the keys <zepto>:status</zepto>, <zepto>:body</zepto> and <zepto>headers</zepto>"]

treatResponse :: Network.Wreq.Response ByteString -> T.LispVal
treatResponse r =
  T.HashMap $ fromList
        [(T.Atom ":status", T.fromSimple $ T.Number (T.NumS (r ^. responseStatus . statusCode))),
         (T.Atom ":body", T.fromSimple $ T.String $ show (r ^. responseBody)),
         (T.Atom ":headers", T.HashMap $ fromList $ treatHeaders)]
    where treatHeaders = map treatHeader (r ^. responseHeaders)
          treatHeader (header, val) = (T.String $ unpack $ original header,
                                       T.fromSimple $ T.String $ unpack val)

makeBReq :: (String -> Data.ByteString.ByteString -> IO (Response ByteString))
         -> [T.LispVal]
         -> T.IOThrowsError T.LispVal
makeBReq fun [T.SimpleVal (T.String url), T.SimpleVal (T.String body)] = do
  res <- liftIO $ fun url (pack body)
  return $ treatResponse res
makeBReq _ [x, T.SimpleVal (T.String _)] = throwError $ T.TypeMismatch "string" x
makeBReq _ [_, x] = throwError $ T.TypeMismatch "string" x
makeBReq _ x = throwError $ T.NumArgs 1 x

makeReq :: (String -> IO (Response ByteString)) -> [T.LispVal] -> T.IOThrowsError T.LispVal
makeReq fun [T.SimpleVal (T.String url)] = do
  res <- liftIO $ fun url
  return $ treatResponse res
makeReq _ [x] = throwError $ T.TypeMismatch "string" x
makeReq _ x = throwError $ T.NumArgs 1 x

genericReqDoc :: String
genericReqDoc =
  intersperse ["takes a string <par>method</par> and a string <par>url</par> ",
               "and performs a request to that URL. Takes an optional payload.",
               "",
               "params:",
               "- method: the request verb",
               "- url: the URL to access",
               "- payload: optional parameter that will be sent as request body",
               "complexity: dependent on the complexity of the response",
               "returns: a hashmap with the keys <zepto>:status</zepto>, <zepto>:body</zepto> and <zepto>headers</zepto>"]

makeGenericReq :: [T.LispVal] -> T.IOThrowsError T.LispVal
makeGenericReq [T.SimpleVal (T.String method), T.SimpleVal (T.String url)] = do
  res <- liftIO $ (wrap (customMethodWith method)) url
  return $ treatResponse res
makeGenericReq [T.SimpleVal (T.String method),
                T.SimpleVal (T.String url),
                T.SimpleVal (T.String payload)] = do
  res <- liftIO $ (wrap (customPayloadMethodWith method)) url (pack payload)
  return $ treatResponse res
