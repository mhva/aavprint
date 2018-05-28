{-# LANGUAGE OverloadedStrings
  , NoMonomorphismRestriction
  , DeriveDataTypeable
  , FlexibleContexts
  , ScopedTypeVariables
  , ConstraintKinds
  #-}

import           System.IO (stderr, hPutStrLn)
import           System.Environment (getArgs)
import           System.Exit

import           Control.Monad (forM_, mapM, mapM_, liftM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State

import           Control.Monad.Base
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Resource
import           Control.Monad.Exception.IO
import qualified Control.Monad.Exception.Pure as CMEPure

import           Data.Either
import           Data.Time
import           Data.List (find, concat, intersperse)
import           Data.Maybe
import           Data.Typeable

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString as B
import qualified Data.CaseInsensitive as CI

import           Data.Digest.Pure.SHA (sha1, showDigest, bytestringDigest)

import           Network.HTTP (urlEncode)
import           Network.HTTP.Conduit
import           Network (withSocketsDo)

import           ArgumentParser
import           JsHTTP
import           JsParser
import           Model
import           Report

data RuntimeParams = RuntimeParams
  { rtUserName    :: String
  , rtPassword    :: String
  , rtOutputFile  :: String
  , rtStartDate   :: Day
  , rtEndDate     :: Day
  , rtLoginURL    :: String
  , rtApiUrl      :: String
  , rtDirectorURL :: String
  , rtExecuteURL  :: String
  , rtLayout      :: String
  } deriving (Show)

data SQLSession= SQLSession
  { sessionParams  :: RuntimeParams
  , sessionCookies :: CookieJar
  , sessionManager :: Manager
  }

data InvalidCredentialsError = InvalidCredentialsError deriving (Show, Typeable)
data NoStudentsError = NoStudentsError deriving (Show, Typeable)
instance Exception InvalidCredentialsError
instance Exception NoStudentsError

type SQLSessionS l m a = ReaderT SQLSession (EMT l (ResourceT m)) a

die :: (MonadIO m) => String -> m a
die err = do liftIO $ (hPutStrLn stderr err >> exitFailure)

makeRuntimeParams :: ( Throws CommandLineError l
                     , Throws CommandLineLookupError l
                     , Monad m
                     )
                  => [String] -> EMT l m RuntimeParams
makeRuntimeParams stringArgs =
  do args        <- parseArgs argSpec stringArgs
     username    <- lookupStringArg "username" args
     password    <- lookupStringArg "password" args
     outputFile  <- lookupStringArg "output" args
     loginUrl    <- lookupStringArg "login-url" args
     apiUrl      <- lookupStringArg "api-url" args
     directorUrl <- lookupStringArg "director-url" args
     executeUrl  <- lookupStringArg "execute-url" args
     startDate   <- lookupDateArg "start-date" args
     endDate     <- lookupDateArg "end-date" args
     layout      <- lookupSelectArg "report-layout" args
                      `CMEPure.catch`
                        (\(ArgumentDoesNotExist e) -> return "list")
     return RuntimeParams
       { rtUserName    = username
       , rtPassword    = password
       , rtOutputFile  = outputFile
       , rtLoginURL    = loginUrl
       , rtApiUrl      = apiUrl
       , rtDirectorURL = directorUrl
       , rtExecuteURL  = executeUrl
       , rtStartDate   = startDate
       , rtEndDate     = endDate
       , rtLayout      = layout
       }
  where
    argSpec =
       [ ("username"     , ArgTypeString)
       , ("password"     , ArgTypeString)
       , ("output"       , ArgTypeString)
       , ("report-layout", ArgTypeSelect ["list", "grid"])
       , ("start-date"   , ArgTypeDate)
       , ("end-date"     , ArgTypeDate)
       , ("login-url"    , ArgTypeString)
       , ("api-url"      , ArgTypeString)
       , ("director-url" , ArgTypeString)
       , ("execute-url"  , ArgTypeString)
       ]

makeCookies :: Int -> String -> String -> String -> [Cookie]
makeCookies uid uname pass domain =
  [ makeCookie "ys-userId" (B8.pack $ urlEncode $ "n:" ++ show uid)
  , makeCookie "ys-user" (B8.pack $ urlEncode $ "s:" ++ uname)
  , makeCookie "ys-password" (B8.pack $ urlEncode $ "s:" ++ hashPassword pass)
  ]
  where past = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
        -- FIXME: Instead of guessing, use current time plus some safety margin.
        future = UTCTime (fromGregorian 2100 1 1) (secondsToDiffTime 0)
        makeCookie n v = Cookie
          { cookie_name = n
          , cookie_value = v
          , cookie_expiry_time = future
          , cookie_domain = B8.pack $ domain
          , cookie_path = "/"
          , cookie_creation_time = past
          , cookie_last_access_time = past
          , cookie_persistent = True
          , cookie_host_only = False
          , cookie_secure_only = False
          , cookie_http_only = False
          }
        hashPassword = showDigest . sha1 . L8.pack

makeLoginRequest :: (Throws InvalidURLError l, Monad m)
                 => String -> String -> String -> EMT l m Request
makeLoginRequest url uname pass =
  do
    requestTemplate url >>= return . urlEncodedBody params
  where
    shasum = showDigest . sha1 . L8.pack
    params = [ ("l", B8.pack uname), ("p", B8.pack $ shasum pass) ]

runJournalRequest :: ( ThrowsHttpErrors l
                     , ThrowsJsParserLookupErrors l
                     , MonadIO m)
                  => JournalQuery -> SQLSessionS l m JsTypecheckedData
runJournalRequest q =
  do env <- ask
     let jar = sessionCookies env
     let manager = sessionManager env
     let url = rtApiUrl $ sessionParams env
     lift $ (dispatch url q) jar manager >>= \(_, js) -> return js
  where
    -- FIXME: Ignoring parameters for GET request.
    dispatch baseUrl (JournalQueryGet action params spec) =
      executeJsGet (baseUrl ++ "/" ++ action) spec
    dispatch baseUrl (JournalQueryPost action params spec) =
      executeJsPost (baseUrl ++ "/" ++ action) params spec

logIn :: (ThrowsHttpErrors l,
          ThrowsJsParserLookupErrors l,
          Throws InvalidCredentialsError l,
          MonadIO m)
      => RuntimeParams -> Manager -> HttpS l m (SQLSession, Teacher)
logIn rt manager =
  do
    let username = rtUserName rt
    let password = rtPassword rt
    loginReq <- makeLoginRequest (rtLoginURL rt) username password
    response <- executeRequest loginReq manager
    loginJs <- parseJs (prBody response) >>= \plainJs ->
      do typecheck loginSpec plainJs
           `CMEPure.catch` (\(e :: JsTypecheckError) ->
             do -- Check if server returned an empty array.
                -- This usually means that login or password was
                -- incorrect.
                typecheck (JsTypeTuple "" []) plainJs
                  `CMEPure.catch` (\(_ :: JsTypecheckError) ->
                    -- We failed the typecheck again an empty
                    -- array, this means that response format
                    -- has changed. Rethrow the initial exception.
                    CMEPure.throw e
                  )
                CMEPure.throw InvalidCredentialsError
           )

    userId <- liftM fromIntegral (lookupNumber ["teacher", "user-id"] loginJs)
    name <- lookupString ["teacher", "full-name"] loginJs
    id_ <- liftM fromIntegral (lookupNumber ["teacher", "teacher-id"] loginJs)
    session <- return $ SQLSession
      { sessionParams = rt
      , sessionCookies = createCookieJar $
          makeCookies userId username password (B8.unpack $ host loginReq)
      , sessionManager = manager
      }
    dmId <- runReaderT (runJournalRequest userInfoR) session
              >>= liftM fromIntegral . lookupNumber ["user-info", "domain-id"]

    return (session, Teacher { teacherName = name
                             , teacherUserId = userId
                             , teacherId = id_
                             , teacherDomainId = dmId
                             })

fetchClassData :: ( ThrowsHttpErrors l
                  , ThrowsJsParserLookupErrors l
                  , Throws NoStudentsError l
                  , MonadIO m)
               => Teacher -> SQLSessionS l m Class
fetchClassData teacher =
  do let dmId = teacherDomainId teacher
     let tcId = teacherId teacher
     now <- liftIO getCurrentTime
     year <- runJournalRequest (sessionStartR now)
       >>= lift . liftM fromIntegral . lookupNumber ["date", "year"]
     studentsJs <- runJournalRequest (studentListR dmId year now)
     students <- lift $ collect makeStudent studentsJs

     -- Get the class id from an id of the first student in the list.
     -- I couldn't find a better way to get the id, so this ass-backwards
     -- approach had to be taken instead.
     studentId <-
       if not (null students)
         then return $ studentId $ students !! 0
         else lift $ throw NoStudentsError
     classId_ <-
       runJournalRequest (classIdFromStudentIdR studentId dmId year now)
         >>= lift . liftM fromIntegral . lookupNumber ["class-info", "id"]

     subjectsJs <- runJournalRequest (subjectListR classId_ dmId year now)
     subjects <- lift $ collect makeSubject subjectsJs
     return Class
       { classStartingYear = year
       , classId = classId_
       , classStudents = students
       , classSubjects = subjects
       }
  where
    makeStudent js =
      do id_  <- liftM fromIntegral $ lookupNumber ["id"] js
         name <- lookupString ["name"] js
         return Student
           { studentId = id_
           , studentName = name
           }
    makeSubject js =
      do id_ <- liftM fromIntegral $ lookupNumber ["id"] js
         name <- lookupString ["name"] js
         return Subject
           { subjectId = id_
           , subjectName = name
           }

fetchMarks :: (ThrowsHttpErrors l, ThrowsJsParserLookupErrors l, MonadIO m)
           => Class -> SQLSessionS l m [(Student, [(Subject, [Mark])])]
fetchMarks class_ =
  do fetchForAll (classStudents class_)
  where
    format marks =
      map
        (\s -> (s, filter (\m -> markSubjectId m == subjectId s) marks))
        (classSubjects class_)
    fetchForAll xs = fetchForAllVerbose xs 1 (length xs)
    fetchForAllVerbose (x:xs) i outof =
      do liftIO $ hPutStrLn stderr $
           "[" ++ show i ++ "/" ++ show outof ++ "] "
             ++ "Fetching student's grades (Student ID: "
             ++ show (studentId x) ++ ")..."
         marks <- fetchFor x
         remainder <- fetchForAllVerbose xs (succ i) outof
         return $ (x, format marks):remainder
    fetchForAllVerbose [] _ _ = return []
    fetchFor x =
      do let sid = studentId x
         let cid = classId class_
         rt <- (ask >>= \p -> return $ sessionParams p)
         js <- runJournalRequest (studentGradesR cid sid)
         lift $ (collect makeGrade js >>=
                  return . filter (\(Mark _ _ d) -> d >= rtStartDate rt &&
                                                    d <= rtEndDate rt))
    makeGrade js =
      do subjectId <- liftM fromIntegral $ lookupNumber ["subject-id"] js
         grade <- lookupString ["name"] js
         date <- lookupDate ["date"] js
         return Mark { markSubjectId = subjectId
                     , markName = grade
                     , markDate = date
                     }

--test :: (Throws JsParserError l, Throws InvalidURLError l, MonadIO m)
--     => Manager
--     -> ReaderT String (StateT Int (EMT l (ResourceT m))) String
--test manager = do a <- httpLbs "test" manager
--                  b <- lift $ lift $ makeLoginRequest "abc" "abc" "cbc"
--                  undefined

main :: IO ()
main = withSocketsDo $
  do rt <- runEMT $
       (liftIO getArgs >>= makeRuntimeParams)
         `catch` handleCommandLineError
         `catch` handleLookupError
     withManager $ \manager -> do
       runEMT $ (do
           liftIO $
             hPutStrLn stderr $ "Trying to sign in as `" ++ rtUserName rt
                                  ++ "'..."
           (session, teacher) <- logIn rt manager
           liftIO $
             hPutStrLn stderr $ "Successfully signed in "
                                  ++ "(Teacher ID: " ++ show (teacherId teacher)
                                  ++ ", Domain ID: "
                                  ++ show (teacherDomainId teacher)
                                  ++ ")"
           class_ <- runReaderT (fetchClassData teacher) session
           marks <- runReaderT (fetchMarks class_) session
           liftIO $ L8.writeFile (rtOutputFile rt) $
             renderReport $ case rtLayout rt of
                              "list" -> reportAsList marks
                              "grid" -> reportAsGrid marks
           liftIO $ hPutStrLn stderr "Success!"
         ) `catch` handleInvalidURLError
           `catch` handleJsParserError
           `catch` handleJsTypecheckError
           `catch` handleJsLookupError
           `catch` handleHttpException
           `catch` handleInvalidCredentialsError
           `catch` handleNoStudentsError
     return ()
  where
    handleCommandLineError (InvalidDate fmt v) =
      die $ "ERROR: Date '" ++ v ++ "' is not a valid date; \
              \the format is: " ++ fmt ++ " (e.g. 2014-05-25)"
    handleCommandLineError (UnknownArgument n) =
      die $ "ERROR: Unrecognized argument '" ++ n ++ "'"
    handleLookupError (ArgumentDoesNotExist n) =
      die $ "ERROR: Missing argument: --" ++ n
    handleLookupError (ArgumentTypeMismatch exp n) =
      die $ "BUG: Argument type mismatch (expected `" ++ exp ++
              "' for --" ++ n ++ ")"
    handleInvalidURLError (InvalidURLError url) =
      die $ "ERROR: URL `" ++ url ++ "' is not a valid URL"
    handleJsParserError (JsParserError s) =
      die $ "ERROR: Could not parse response from the server"
    handleJsTypecheckError (e :: JsTypecheckError) =
      die $ "ERROR: Server response did not pass a typecheck"
    handleJsLookupError (e :: JsLookupError) =
      die $ "BUG: Lookup in structured data has failed"
    handleHttpException (e :: HttpException) =
      die $ "ERROR: An error occurred while running an HTTP request: "
              ++ show e
    handleInvalidCredentialsError (e :: InvalidCredentialsError) =
      die $ "ERROR: Unable not sign in: login or password is incorrect"
    handleNoStudentsError (e :: NoStudentsError) =
      die $ "ERROR: Server returned an empty list of students; \
              \couldn't extract class id"
