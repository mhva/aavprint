{-# LANGUAGE OverloadedStrings
  , NoMonomorphismRestriction
  , DeriveDataTypeable
  , FlexibleContexts
  , ScopedTypeVariables
  , ConstraintKinds
  #-}

module JsHTTP ( PlainResponse (..)
              , HttpS (..)
              , StructuredResponse (..)
              , ThrowsJsParserErrors (..)
              , ThrowsJsLookupErrors (..)
              , ThrowsJsParserLookupErrors (..)
              , ThrowsHttpErrors (..)
              , InvalidURLError (..)
              , requestTemplate
              , executeJsPost
              , executeJsGet
              , executeJsRequest
              , executeRequest
              ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Exception
import Control.Monad.Trans
import Control.Monad.Trans.Resource

import Data.List
import Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8

import Data.Encoding

import Text.Parsec
import Text.Parsec.String

import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status (Status)

import JsParser

data InvalidURLError = InvalidURLError String deriving (Typeable, Show)
instance Exception InvalidURLError

data PlainResponse = PlainResponse
  { prHeaders :: ResponseHeaders
  , prBody :: String
  , prStatus :: Status
  , prCookieJar :: CookieJar
  }

type HttpS l m a = EMT l (ResourceT m) a
type StructuredResponse = (PlainResponse, JsTypecheckedData)
type ThrowsJsParserErrors l = (Throws JsParserError l, Throws JsTypecheckError l)
type ThrowsJsLookupErrors l = (Throws JsLookupError l)
type ThrowsJsParserLookupErrors l = (ThrowsJsParserErrors l, ThrowsJsLookupErrors l)
type ThrowsHttpErrors l = (Throws InvalidURLError l, Throws HttpException l)

requestTemplate :: (Throws InvalidURLError l, Monad m)
                => String -> EMT l m Request
requestTemplate url =
  do either
      (\_ -> throw $ InvalidURLError url)
      return
      (parseUrl url >>= return . updateHeaders)
  where
    updateHeaders req = req
      { requestHeaders =
          [ (hUserAgent, "Mozilla/5.0 (Windows NT 6.1; rv:31.0) Gecko\
                         \/20100101 Firefox/31.0")
          , (hAccept, "text/html,application/xhtml+xml,application/\
                      \xml;q=0.9,*/*;q=0.8")
          , (hAcceptLanguage, "en-US,en;q=0.5")
          , ("Accept-Encoding", "gzip")
          , ("X-Requested-With", "XMLHttpRequest")
          , ("Pragma", "no-cache")
          , (hCacheControl, "no-cache")
          ]
      , cookieJar = Just $ createCookieJar []
      }

executeJsPost :: (ThrowsHttpErrors l , ThrowsJsParserLookupErrors l , MonadIO m)
              => String
              -> [(String, String)]
              -> JsTypeSpec
              -> CookieJar
              -> Manager
              -> HttpS l m StructuredResponse
executeJsPost url params spec cookies manager =
  do req <- makePostRequest url params cookies
     executeJsRequest req spec manager
  where
    makePostRequest url params cookies =
      do
        let bsParams = map (\(n, v) -> (B8.pack n, B8.pack v)) params
        -- We have to override the Content-Type header because urlEncodedBody
        -- does not let us specify the charset.
        requestTemplate url >>= (return . urlEncodedBody bsParams) >>= \req ->
          return $ req
            { requestHeaders =
                ("Content-Type", "application/x-www-form-urlencoded;\
                                 \ charset=utf-8") :
                filter (\(n, _) -> n /= "Content-Type") (requestHeaders req)
            , cookieJar = Just cookies
            }

executeJsGet :: (ThrowsHttpErrors l , ThrowsJsParserLookupErrors l , MonadIO m)
              => String
              -> JsTypeSpec
              -> CookieJar
              -> Manager
              -> HttpS l m StructuredResponse
executeJsGet url spec cookies manager =
  do req <- makeGetRequest url cookies
     executeJsRequest req spec manager
  where
    makeGetRequest url cookies =
      requestTemplate url >>= \req ->
        return $ req { method = "GET", cookieJar = Just cookies }

executeJsRequest :: (ThrowsJsParserErrors l, Throws HttpException l, MonadIO m)
                 => Request -> JsTypeSpec -> Manager
                 -> HttpS l m StructuredResponse
executeJsRequest req spec manager =
  do response <- executeRequest req manager
     js       <- parseJs (prBody response) >>= typecheck spec
     return (undefined, js)

parseContentType :: String -> Maybe (String, String, [(String, String)])
parseContentType ct =
  either (\_ -> Nothing) return (parse parser "(unknown)" ct)
  where
    parser =
      do type_ <- many alphaNum
         char '/'
         subtype <- many alphaNum
         spaces
         params <- many params
         return (type_, subtype, params)
    params =
      do char ';' >> spaces
         name <- many alphaNum
         spaces >> char '=' >> spaces
         -- FIXME: May not handle all possible parameter values.
         value <-  between (char '"') (char '"') (many $ noneOf "\"")
               <|> many (alphaNum <|> char '-')
         return (name, value)

executeRequest :: (Throws HttpException l, MonadIO m)
               => Request -> Manager -> HttpS l m PlainResponse
executeRequest req manager =
  do resp <- retryHttp 5 req
     body <- return $ decodeBody resp
     return $ PlainResponse
                { prStatus = responseStatus resp
                , prHeaders = responseHeaders resp
                , prBody = body
                , prCookieJar = responseCookieJar resp
                }
  where
    retryHttp tries req = ((lift $ httpLbs req manager)
                            `catch` \(e :: HttpException) -> do
                              if pred tries > 0
                                then do liftIO $ threadDelay 5000
                                        retryHttp (pred tries) req
                                else throw e)
    selectEncoding r =
      do (_, ct) <- find (\(n, _) -> n == "Content-Type") (responseHeaders r)
         (t, st, params) <- parseContentType $ B8.unpack ct
         (_, encName) <- find (\(n, _) -> n == "charset") params
         encodingFromStringExplicit encName

    decodeBodyDefaultCharset r =
      decodeLazyByteString (encodingFromString "latin-1") (responseBody r)
    decodeBody r =
      case selectEncoding r of
        Nothing -> decodeBodyDefaultCharset r
        Just enc -> either (\_ -> decodeBodyDefaultCharset r)
                           (id)
                           (decodeLazyByteStringExplicit enc body)
      where body = responseBody r
