module Model ( Teacher (..)
             , Student (..)
             , Subject (..)
             , Class (..)
             , Mark (..)
             , JournalQuery (..)
             , loginSpec
             , userInfoR
             , sessionStartR
             , studentListR
             , subjectListR
             , classIdFromStudentIdR
             , studentGradesR
             ) where

import Data.Time
import Data.List (concat, intersperse)

import JsParser

data Teacher = Teacher
  { teacherName     :: String
  , teacherUserId   :: Int
  , teacherId       :: Int
  , teacherDomainId :: Int
  } deriving (Show)

data Student = Student
  { studentId :: Int
  , studentName :: String
  } deriving (Show)

data Subject = Subject
  { subjectId :: Int
  , subjectName :: String
  } deriving (Show)

data Class = Class
  { classId :: Int
  , classStartingYear :: Int
  , classSubjects     :: [Subject]
  , classStudents     :: [Student]
  } deriving (Show)

data Mark = Mark
  { markSubjectId :: Int
  , markName :: String
  , markDate :: Day
  } deriving (Show)

data JournalQuery
  = JournalQueryGet String [(String, String)] JsTypeSpec
  | JournalQueryPost String [(String, String)] JsTypeSpec
  deriving (Show)

formatDate :: Day -> String
formatDate = toString . toGregorian
  where toString (year, month, day) =
          concat $ intersperse "." [show day, show month, show year]

loginSpec =
  JsTypeTuple "root"
    [ JsTypeIndexedTuple "teacher"
      [ (0, JsTypeNumber "user-id")
      , (5, JsTypeString "full-name")
      , (7, JsTypeNumber "teacher-id")
      ]
    ]

userInfoR :: JournalQuery
userInfoR = JournalQueryGet a p s
  where
    a = "get_user_data"
    p = []
    s = JsTypeTuple "root"
          [ JsTypeIndexedTuple "user-info"
            [ (0, JsTypeString "last-name")
            , (1, JsTypeString "first-name")
            , (2, JsTypeString "patronymic")
            , (4, JsTypeNumber "domain-id")
            ]
          ]

sessionStartR :: UTCTime -> JournalQuery
sessionStartR now = JournalQueryPost a p s
  where
    a = "get_uch_year"
    p = [("currentDate", formatDate (utctDay now))]
    s = JsTypeTuple "root" [JsTypeTuple "date" [JsTypeNumber "year"]]

studentListR :: Int -> Int -> UTCTime -> JournalQuery
studentListR teacherDomainId sessionStart now = JournalQueryPost a p s
  where
    a = "get_students_list_data"
    p = [ ("uchId", show teacherDomainId)
        , ("currentDate", formatDate (utctDay now))
        , ("uchYear", show sessionStart)
        ]
    s = JsTypeRepeat "root"
          (JsTypeTuple "student"
            [ JsTypeNumber "id"
            , JsTypeString "name"
            ])

subjectListR :: Int -> Int -> Int -> UTCTime -> JournalQuery
subjectListR classId domainId sessionStart now = JournalQueryPost a p s
  where
    a = "get_class_subjects"
    p = [ ("cls", show classId)
        , ("isClassTeam", "false")
        , ("subsSubjectIds", "")
        , ("subsClassIds", "")
        , ("uchId", show domainId)
        , ("currentDate", formatDate (utctDay now))
        , ("uchYear", show sessionStart)
        ]
    s = JsTypeRepeat "root"
          (JsTypeIndexedTuple "subject"
            [ (0, JsTypeNumber "id") -- XXX: Not sure, if this is the actual id
                                     -- or not.
            , (1, JsTypeString "name")
            ])

classIdFromStudentIdR :: Int -> Int -> Int -> UTCTime -> JournalQuery
classIdFromStudentIdR studentId domainId year now = JournalQueryPost a p s
  where
    a = "get_student_class"
    p = [ ("currentDate", formatDate (utctDay now))
        , ("student", show studentId)
        , ("uchYear", show year)
        , ("uchId", show domainId)
        ]
    s = JsTypeTuple "root" [
          JsTypeIndexedTuple "class-info"
            [ (0, JsTypeNumber "id")
            ]]

studentGradesR :: Int -> Int -> JournalQuery
studentGradesR classId studentId = JournalQueryPost a p s
  where
    a = "get_student_journal_data"
    p = [ ("cls", show classId)
        , ("parallelClasses", "")
        , ("student", show studentId)
        ]
    s = JsTypeRepeat "root"
          (JsTypeIndexedTuple "grade"
            [ (2, JsTypeString "name")
            , (3, JsTypeDate "date")
            , (4, JsTypeNumber "subject-id")
            ])
