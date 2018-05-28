{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

module ArgumentParser ( CommandLineArgType(..)
                      , CommandLineError(..)
                      , CommandLineLookupError(..)
                      , ArgumentList
                      , parseArgs
                      , lookupStringArg
                      , lookupDateArg
                      , lookupSelectArg
                      ) where

import Control.Monad (Monad, sequence)
import Control.Monad.Exception

import Data.List (find)
import Data.Maybe
import Data.Time
import Data.Typeable
import Data.String.Utils (startswith, split)

import Text.Read (readMaybe)

data CommandLineArgType
    = ArgTypeString
    | ArgTypeDate
    | ArgTypeSelect [String]
    deriving (Show)

data CommandLineArg
    = ArgString String
    | ArgDate Day
    | ArgSelect String
    deriving (Show)

newtype ArgumentList = ArgumentList [(String, CommandLineArg)] deriving (Show)

data CommandLineError
    = InvalidDate String String
    | UnknownArgument String
    | InvalidSelect String [String]
    deriving (Show, Typeable)

data CommandLineLookupError
    = ArgumentDoesNotExist String
    | ArgumentTypeMismatch String String
    deriving (Show, Typeable)

instance Exception CommandLineError
instance Exception CommandLineLookupError

parseArgs :: (Throws CommandLineError l, Monad m)
          => [(String, CommandLineArgType)]
          -> [String]
          -> EMT l m ArgumentList
parseArgs typeMap args = parseArgs' args >>= return . ArgumentList
  where parseDate s =
          let
            components =
              sequence $ map (\x -> readMaybe x :: Maybe Int) $ split "-" s
          in if isJust components && length (fromJust components) == 3
               then return (ArgDate $ fromGregorian
                             (toInteger $ (fromJust components) !! 0)
                             ((fromJust components) !! 1)
                             ((fromJust components) !! 2))
               else throw $ InvalidDate "yyyy-mm-dd" s
        parseSelect allowedValues s =
          maybe (throw $ InvalidSelect s allowedValues)
                (return . ArgSelect)
                (find (== s) allowedValues)
        chooseParser type_ = case type_ of
                               ArgTypeString    -> return . ArgString
                               ArgTypeDate      -> parseDate
                               ArgTypeSelect xs -> parseSelect xs
        parserList = map (\(n, t) -> ("--" ++ n ++ "=", n, chooseParser t)) typeMap
        matchParser arg = find (\(pre, _, _) -> startswith pre arg) parserList
        parseArgs' (x:xs) =
          maybe
            (throw $ UnknownArgument x)
            (\(pre, n, f) -> do value <- f (drop (length pre) x)
                                remainder <- parseArgs' xs
                                return $ (n, value):remainder)
            (matchParser x)
        parseArgs' [] = return []

lookupArg :: (Throws CommandLineLookupError l, Monad m)
          => String -> ArgumentList -> EMT l m CommandLineArg
lookupArg name (ArgumentList xs) =
  maybe (throw $ ArgumentDoesNotExist name)
        (\(_, v) -> return v)
        (find (\(n, _) -> n == name) xs)

lookupStringArg :: (Throws CommandLineLookupError l, Monad m)
                => String -> ArgumentList -> EMT l m String
lookupStringArg name args = lookupArg name args >>= \value ->
                              case value of
                                ArgString s -> return s
                                _ -> throw $ ArgumentTypeMismatch "String" name

lookupDateArg :: (Throws CommandLineLookupError l, Monad m)
              => String -> ArgumentList -> EMT l m Day
lookupDateArg name args = lookupArg name args >>= \value ->
                            case value of
                              ArgDate d -> return d
                              _ -> throw $ ArgumentTypeMismatch "Date" name

lookupSelectArg :: (Throws CommandLineLookupError l, Monad m)
                => String -> ArgumentList -> EMT l m String
lookupSelectArg name args = lookupArg name args >>= \value ->
                              case value of
                                ArgSelect s -> return s
                                _ -> throw $ ArgumentTypeMismatch "Select" name
