{-# LANGUAGE OverloadedStrings
  , NoMonomorphismRestriction
  , FlexibleContexts
  , DeriveDataTypeable
  , PatternGuards
  #-}

module JsParser ( JsType (..)
                , JsTypeSpec (..)
                , JsTypecheckedData
                , JsParserError (..)
                , JsTypecheckError (..)
                , JsLookupError (..)
                , parseJs
                , typecheck
                , lookupString
                , lookupNumber
                , lookupDate
                , collect
                ) where

import Control.Monad (liftM)
import Control.Monad.Exception

import Data.Typeable
import Data.Either
import Data.List (maximumBy, sortBy, find)
import Data.String (IsString)
import Data.Maybe
import Data.Time

import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

data JsType
    = JsString String
    | JsNumber Integer
    | JsArray [JsType]
    | JsDate Day
    | JsNull
    deriving (Show)

data JsTypeSpec
    = JsTypeString String
    | JsTypeNumber String
    | JsTypeDate String
    | JsTypeTuple String [JsTypeSpec]
    | JsTypeIndexedTuple String [(Int, JsTypeSpec)]
    | JsTypeRepeat String JsTypeSpec
    | JsTypeDontCare
      deriving (Show)

data JsTypecheckedData
    = JsTcString String String
    | JsTcNumber String Integer
    | JsTcDate String Day
    | JsTcArray String [JsTypecheckedData]
    | JsTcAny JsType
      deriving (Show)

data JsParserError = JsParserError String deriving (Show, Typeable)

data JsTypecheckError
    = TypecheckExpectedArrayTypeError JsType
    | TypecheckExpectedStringTypeError JsType
    | TypecheckExpectedNumberTypeError JsType
    | TypecheckExpectedDateTypeError JsType
    | TypecheckArrayLengthMismatchError JsType
      deriving (Show, Typeable)

data JsLookupError
    = KeyNotFoundError [String]
    | LookupTypeMismatchError JsTypecheckedData
      deriving (Show, Typeable)

instance Exception JsParserError
instance Exception JsLookupError
instance Exception JsTypecheckError

natural = P.natural (P.makeTokenParser haskellDef)

jsNum = liftM JsNumber $ P.integer (P.makeTokenParser haskellDef)

jsString = do between (char '"') (char '"') doubleQuot
          <|> between (char '\'') (char '\'') singleQuot
  where
    singleQuot = liftM JsString $ (many $ jchar '\'')
    doubleQuot = liftM JsString $ (many $ jchar '"')
    jchar quot = escaped <|> noneOf [quot]
    escaped = char '\\' >> (choice (map unescape specials) <|> return '\\')
    unescape (c, repl) = char c >> return repl
    specials = [ ('\\', '\\'), ('"', '"'), ('\'', '\''), ('n', '\n')
               , ('r', '\r'), ('t', '\t'), ('b', '\b'), ('f', '\f')
               , ('v', '\v') ]

jsArray = do between (char '[' >> spaces) (spaces >> char ']') array
  where
    array = liftM JsArray $ sepBy element (char ',')
    element = between spaces spaces jsValue

jsValue = jsArray <|> jsString <|> jsDate <|> jsNull <|> jsNum

jsDate =  try $ string "new" >> spaces >> string "Date"
       >> spaces >> between (char '(') (char ')') extract
  where
    extract = do spaces
                 year <- natural
                 spaces >> char ',' >> spaces
                 -- Months in JavaScript are zero-based.
                 month <- liftM ((1 +) . fromIntegral) natural
                 spaces >> char ',' >> spaces
                 day <- liftM fromIntegral natural
                 many $ (digit <|> char ',')
                 return $ JsDate $ fromGregorian year month day

jsNull = try $ string "null" >> return JsNull

typecheck :: (Throws JsTypecheckError l, Monad m)
          => JsTypeSpec -> JsType -> EMT l m JsTypecheckedData
typecheck spec dat | JsTypeRepeat _ _ <- spec = typecheckRepeated spec dat
                   | JsTypeTuple _ _ <- spec = typecheckTuple spec dat
                   | JsTypeIndexedTuple _ _ <- spec = typecheckITuple spec dat
                   | JsTypeString _ <- spec = typecheckString spec dat
                   | JsTypeNumber _ <- spec = typecheckNumber spec dat
                   | JsTypeDate _ <- spec = typecheckDate spec dat
                   | JsTypeDontCare <- spec = typecheckDontCare spec dat
  where
    typecheckRepeated (JsTypeRepeat name inner) (JsArray xs) =
      let typecheckElem x acc = do accumulator <- acc
                                   tcResult <- typecheck inner x
                                   return (tcResult:accumulator)
      in do liftM (JsTcArray name) (foldr typecheckElem (return []) xs)
    typecheckRepeated _ js = throw $ TypecheckExpectedArrayTypeError js

    typecheckTuple (JsTypeTuple name specs) js@(JsArray xs)
      | length specs /= length xs = throw $ TypecheckArrayLengthMismatchError js
      | otherwise = do liftM (JsTcArray name) (matchEqualArrays specs xs)
    typecheckTuple _ js = throw $ TypecheckExpectedArrayTypeError js
    matchEqualArrays (s:specs) (x:xs) = do matchList <- matchEqualArrays specs xs
                                           match <- typecheck s x
                                           return (match:matchList)
    matchEqualArrays [] [] = return []

    typecheckITuple (JsTypeIndexedTuple name specs) js@(JsArray xs) =
      do let types = map snd specs
         let (maxIndex, _) = maximumBy (\(x, _) (y, _) -> compare x y) specs
         let values = map (\(i, _) -> xs !! i) specs
         if maxIndex < length xs
           then liftM (JsTcArray name) (matchEqualArrays types values)
           else throw $ TypecheckArrayLengthMismatchError js
    typecheckITyples _ js = throw $ TypecheckExpectedArrayTypeError js

    typecheckString (JsTypeString n) (JsString v) = return $ JsTcString n v
    typecheckString _ js = throw $ TypecheckExpectedStringTypeError js

    typecheckNumber (JsTypeNumber n) (JsNumber v) = return $ JsTcNumber n v
    typecheckNumber _ js = throw $ TypecheckExpectedNumberTypeError js

    typecheckDate (JsTypeDate n) (JsDate v) = return $ JsTcDate n v
    typecheckDate _ js = throw $ TypecheckExpectedDateTypeError js

    typecheckDontCare JsTypeDontCare v = return $ JsTcAny v

lookupPath :: [String] -> JsTypecheckedData -> Maybe JsTypecheckedData
lookupPath (key:remainder) arr@(JsTcArray _ _) =
    maybe Nothing (lookupPath remainder) (lookupInArray key arr)
  where
    name (JsTcArray n _) = Just n
    name (JsTcString n _) = Just n
    name (JsTcNumber n _) = Just n
    name (JsTcDate n _) = Just n
    name (JsTcAny _) = Nothing
    lookupInArray key (JsTcArray _ xs) = find (maybe False (== key) . name) xs
lookupPath [] v = Just v

lookupValue :: (Throws JsLookupError l, Monad m)
            => [String]
            -> (JsTypecheckedData -> Maybe a)
            -> JsTypecheckedData
            -> EMT l m a
lookupValue path unbox dat =
  do js <- maybe (throw $ KeyNotFoundError path)
                 return
                 (lookupPath path dat)
     case unbox js of
       Just v  -> return v
       Nothing -> throw $ LookupTypeMismatchError js

lookupString :: (Throws JsLookupError l, Monad m)
             => [String] -> JsTypecheckedData -> EMT l m String
lookupString path dat = lookupValue path unbox dat
  where unbox (JsTcString _ v) = Just v
        unbox _                = Nothing

lookupNumber :: (Throws JsLookupError l, Monad m)
             => [String] -> JsTypecheckedData ->EMT l m Integer
lookupNumber path dat = lookupValue path unbox dat
  where unbox (JsTcNumber _ v) = Just v
        unbox _                = Nothing

lookupDate :: (Throws JsLookupError l, Monad m)
           => [String] -> JsTypecheckedData -> EMT l m Day
lookupDate path dat = lookupValue path unbox dat
  where unbox (JsTcDate _ v) = Just v
        unbox _              = Nothing

collect :: (Throws JsLookupError l, Monad m)
        => (JsTypecheckedData -> EMT l m a) -> JsTypecheckedData -> EMT l m [a]
collect f (JsTcArray _ arr) = sequence $ collect' arr
  where collect' (x:xs) = f x : collect' xs
        collect' []     = []
collect f t = throw $ LookupTypeMismatchError t

parseJs :: (Throws JsParserError l, Monad m) => String -> EMT l m JsType
parseJs s = either
              (\err -> throw $ JsParserError $ show err)
              return
              (parse jsValue "(unknown)" s)
