{-# LANGUAGE OverloadedStrings #-}
module Report ( reportAsList
              , reportAsGrid
              , renderReport
              ) where

import           Prelude hiding (head, div)
import qualified Prelude as P

import qualified Data.ByteString.Lazy.Char8 as L8

import           Data.Maybe
import           Data.Time
import           Data.List (find, intersperse, concat)
import           Text.Read (readMaybe)
import           Numeric (showFFloat)

import           Text.Blaze.Html5 hiding (map)
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import Model

newtype Report = Report Html

gridCss :: Html
gridCss =
  "html,body,div,span,applet,object,iframe,h1,h2,h3,h4,h5,h6,p,blockquote,pre,a,abbr,acronym,address,big,cite,code,del,dfn,em,img,ins,kbd,q,s,samp,small,strike,strong,sub,sup,tt,var,b,u,i,center,dl,dt,dd,ol,ul,li,fieldset,form,label,legend,table,caption,tbody,tfoot,thead,tr,th,td,article,aside,canvas,details,embed,figure,figcaption,footer,header,hgroup,menu,nav,output,ruby,section,summary,time,mark,audio,video{border:0;font-size:100%;font:inherit;vertical-align:baseline;margin:0;padding:0}article,aside,details,figcaption,figure,footer,header,hgroup,menu,nav,section{display:block}body{line-height:1}ol,ul{list-style:none}blockquote,q{quotes:none}blockquote:before,blockquote:after,q:before,q:after{content:none}table{border-collapse:collapse;border-spacing:0} \
  \body { \
  \  background: #fff;\
  \  font: 14px/21px \"HelveticaNeue\", \"Helvetica Neue\", Helvetica, Verdana, sans-serif;\
  \  color: #333;\
  \  -webkit-font-smoothing: antialiased;\
  \  -webkit-text-size-adjust: 100%;\
  \} \
  \h1, h2, h3, h4, h5, h6 { \
  \  color: #181818;\
  \  font-family: \"Georgia\", \"Times New Roman\", serif;\
  \  font-weight: normal;\
  \} \
  \h1 a, h2 a, h3 a, h4 a, h5 a, h6 a { font-weight: inherit; } \
  \h1 { font-size: 46px; line-height: 50px; margin-bottom: 14px; } \
  \h2 { font-size: 35px; line-height: 40px; margin-bottom: 10px; } \
  \h3 { font-size: 28px; line-height: 34px; margin-bottom: 8px; } \
  \h4 { font-size: 21px; line-height: 30px; margin-bottom: 4px; } \
  \h5 { font-size: 17px; line-height: 24px; } \
  \h6 { font-size: 14px; line-height: 21px; } \
  \p { margin: 0 0 20px 0; } \
  \ \
  \.pair { \
  \  clear: both;\
  \} \
  \.pair:after { \
  \  /* Breaks printing in Firefox */\
  \  /*content: \"\";\
  \  clear: both;\
  \  display: block;*/\
  \} \
  \.fst-half { \
  \  float: left;\
  \} \
  \.snd-half { \
  \  float: right;\
  \} \
  \.spreadsheet { \
  \  page-break-inside: avoid;\
  \  margin-bottom: 0.3cm;\
  \  width: 300px;\
  \} \
  \.spreadsheet .name { margin-bottom: 6px; } \
  \.spreadsheet table { \
  \  font-size: 9px;\
  \  font-family: Verdana !important; /* XXX: Hack to make Chrome happy */\
  \  border-bottom: 1px solid;\
  \} \
  \.spreadsheet th { \
  \  text-align: left;\
  \  border-top: 1px solid;\
  \  border-bottom: 1px solid;\
  \  vertical-align: middle;\
  \} \
  \.spreadsheet th:not(:first-child) { padding: 0 10px; } \
  \.spreadsheet td { \
  \  line-height: 1.5em;\
  \  border-bottom: 1px dotted;\
  \  letter-spacing: -1px;\
  \  vertical-align: middle;\
  \} \
  \.spreadsheet td.subject { width: 150px; } \
  \.spreadsheet td.marks { width: 400px; } \
  \.spreadsheet td.avg { text-align: right; } \
  \.spreadsheet td:not(:first-child) { padding: 0 10px; } \
  \ "

reportAsGrid' :: [ (Student, [ (Subject, [ Mark ]) ]) ] -> Html
reportAsGrid' grades = docTypeHtml $ do
  head $ do
    title "Student Grades Breakdown"
    meta ! A.charset "utf-8"
    style gridCss
  body $ do
    renderPairs $ breakInPairs grades
  where
    breakInPairs (x1:x2:xs) = (Just x1, Just x2) : breakInPairs xs
    breakInPairs (x1:[])    = (Just x1, Nothing) : []
    breakInPairs []         = []
    renderPairs ((x1, x2):xs) =
      do div ! A.class_ "pair" $ do
           maybe (return ()) ((div ! A.class_ "fst-half") . renderStudent) x1
           maybe (return ()) ((div ! A.class_ "snd-half") . renderStudent) x2
         renderPairs xs
    renderPairs [] = return ()
    renderStudent (student, marks) =
      do div ! A.class_ "spreadsheet" $ do
           h6 ! A.class_ "name" $ toHtml $ studentName student
           table $ do
             thead $ tr $ do
               th "Предмет"
               th "Оценки"
               th "Average"
             tbody $ makeRows marks
    makeRows ((subj, marks):xs) =
      do tr $ do
           td ! A.class_ "subject" $ toHtml $ name
           td ! A.class_ "marks" $ toHtml $
             concat $ intersperse " " $ map markName marks
           td ! A.class_ "avg" $ toHtml $
             if isInfinite avg || isNaN avg
               then "—"
               else showFFloat (Just 2) avg ""
         makeRows xs
      where
        avg = let toNumber x = readMaybe (markName x) :: Maybe Int
                  actualMarks = catMaybes $ map toNumber marks
                  sum_ = sum actualMarks
                  len = length actualMarks
              in (fromIntegral sum_ / fromIntegral len) :: Float
        nameMap = [("Основы безопасности жизнедеятельности (ОБЖ)", "ОБЖ")]
        name = maybe (subjectName subj)
                     (\(_, v) -> v)
                     (find (\(n, _) -> n == subjectName subj) nameMap)
    makeRows [] = return ()

listCss :: Html
listCss =
  "html,body,div,span,applet,object,iframe,h1,h2,h3,h4,h5,h6,p,blockquote,pre,a,abbr,acronym,address,big,cite,code,del,dfn,em,img,ins,kbd,q,s,samp,small,strike,strong,sub,sup,tt,var,b,u,i,center,dl,dt,dd,ol,ul,li,fieldset,form,label,legend,table,caption,tbody,tfoot,thead,tr,th,td,article,aside,canvas,details,embed,figure,figcaption,footer,header,hgroup,menu,nav,output,ruby,section,summary,time,mark,audio,video{border:0;font-size:100%;font:inherit;vertical-align:baseline;margin:0;padding:0}article,aside,details,figcaption,figure,footer,header,hgroup,menu,nav,section{display:block}body{line-height:1}ol,ul{list-style:none}blockquote,q{quotes:none}blockquote:before,blockquote:after,q:before,q:after{content:none}table{border-collapse:collapse;border-spacing:0} \
  \body { \
  \  background: #fff;\
  \  font: 14px/21px \"HelveticaNeue\", \"Helvetica Neue\", Helvetica, Verdana, sans-serif;\
  \  color: #333;\
  \  -webkit-font-smoothing: antialiased;\
  \  -webkit-text-size-adjust: 100%;\
  \} \
  \h1, h2, h3, h4, h5, h6 { \
  \  color: #181818;\
  \  font-family: \"Georgia\", \"Times New Roman\", serif;\
  \  font-weight: normal;\
  \} \
  \h1 a, h2 a, h3 a, h4 a, h5 a, h6 a { font-weight: inherit; } \
  \h1 { font-size: 46px; line-height: 50px; margin-bottom: 14px; } \
  \h2 { font-size: 35px; line-height: 40px; margin-bottom: 10px; } \
  \h3 { font-size: 28px; line-height: 34px; margin-bottom: 8px; } \
  \h4 { font-size: 21px; line-height: 30px; margin-bottom: 4px; } \
  \h5 { font-size: 17px; line-height: 24px; } \
  \h6 { font-size: 14px; line-height: 21px; } \
  \p { margin: 0 0 20px 0; } \
  \ \
  \.spreadsheet { \
  \  width: 75%;\
  \  margin: 0 auto;\
  \  margin-bottom: 0.3cm;\
  \  page-break-inside: avoid;\
  \} \
  \.spreadsheet .name { margin-bottom: 6px; } \
  \.spreadsheet table { \
  \  font-size: 10px;\
  \  border-bottom: 1px solid;\
  \  font-family: Verdana !important; /* XXX: Hack to make Chrome happy */\
  \} \
  \.spreadsheet th { \
  \  text-align: left;\
  \  border-top: 1px solid;\
  \  border-bottom: 1px solid;\
  \  vertical-align: middle;\
  \} \
  \.spreadsheet th:not(:first-child) { padding: 0 10px; } \
  \.spreadsheet td { \
  \  line-height: 1.5em;\
  \  border-bottom: 1px dotted;\
  \  letter-spacing: -1px;\
  \  vertical-align: middle;\
  \} \
  \.spreadsheet td.subject { width: 150px; } \
  \.spreadsheet td.marks { width: 400px; } \
  \.spreadsheet td.avg { text-align: right; } \
  \.spreadsheet td:not(:first-child) { padding: 0 10px; } \
  \ "

reportAsList' :: [ (Student, [ (Subject, [ Mark ]) ]) ] -> Html
reportAsList' grades = docTypeHtml $ do
  head $ do
    title "Student Grades Breakdown"
    meta ! A.charset "utf-8"
    style listCss
  body $ do
    makeSpreadsheet grades
  where
    makeSpreadsheet ((student, marks):xs) =
      do div ! A.class_ "spreadsheet" $ do
           h6 ! A.class_ "name" $ toHtml $ studentName student
           table $ do
             thead $ tr $ do
               th "Название предмета"
               th "Оценки"
               th "Average"
             tbody $ makeRows marks
         makeSpreadsheet xs
    makeSpreadsheet [] = return ()
    makeRows ((subj, marks):xs) =
      do tr $ do
           td ! A.class_ "subject" $ toHtml $ name
           td ! A.class_ "marks" $ toHtml $
             concat $ intersperse " " $ map markName marks
           td ! A.class_ "avg" $ toHtml $
             if isInfinite avg || isNaN avg then "—"
                                            else showFFloat (Just 2) avg ""
         makeRows xs
      where avg = let toNumber x = readMaybe (markName x) :: Maybe Int
                      actualMarks = catMaybes $ map toNumber marks
                      sum_ = sum actualMarks
                      len = length actualMarks
                  in (fromIntegral sum_ / fromIntegral len) :: Float
            name = let nameMap = [("Основы безопасности жизнедеятельности (ОБЖ)", "ОБЖ")]
                   in maybe (subjectName subj)
                            (\(_, v) -> v)
                            (find (\(n, _) -> n == subjectName subj) nameMap)

    makeRows [] = return ()

reportAsList :: [ (Student, [ (Subject, [Mark]) ]) ] -> Report
reportAsList = Report . reportAsList'

reportAsGrid :: [ (Student, [ (Subject, [Mark]) ]) ] -> Report
reportAsGrid = Report . reportAsGrid'

renderReport :: Report -> L8.ByteString
renderReport (Report html) = renderHtml html
