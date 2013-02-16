{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts #-}
module Handler.BBS
       ( getBBSR
       , postBBSR
       , getThreadR
       , postThreadR
       ) where

import Import hiding (migrateAll)
import Yesod.Form.Nic (nicHtmlField)
import Data.Time

getBBSR :: Handler RepHtml
getBBSR = do
  threads <- runDB $ selectList [] [Desc ThreadTitle]
  (threadWidget, enctype) <- generateFormPost threadForm
  defaultLayout $ do
    $(widgetFile "index")
    setTitle "Thread archive"
    $(widgetFile "list-layout")

postBBSR :: Handler RepHtml
postBBSR = do
  ((res, threadWidget), enctype) <- runFormPost threadForm
  case res of
    FormSuccess thread -> do
      threadId <- runDB $ insert thread
      setMessage $ toHtml $ (threadTitle thread) <> " created"
      redirect $ ThreadR threadId
    _ -> defaultLayout $ do
      setTitle "Please correct your thread form"
      $(widgetFile "threadAddError")

threadForm :: Form Thread
threadForm = renderDivs $ Thread
    <$> areq textField "Title" Nothing
    <*> aformM (liftIO getCurrentTime)
    <*> areq nicHtmlField "Content" Nothing
    
getThreadR :: ThreadId -> Handler RepHtml
getThreadR threadId = do
  thread <- runDB $ get404 threadId
  
  replies <- runDB $ selectList [] [Desc ReplyTitle]
  (replyWidget, enctype) <- generateFormPost replyForm
  
  defaultLayout $ do
    setTitle $ toHtml $ "Thread: #" <> (pack . show $ threadId) <> " " <> (threadTitle thread)
    $(widgetFile "index")
    $(widgetFile "thread-layout")

postThreadR :: ThreadId -> Handler RepHtml
postThreadR threadId = do
  ((res, replyWidget), enctype) <- runFormPost replyForm
  case res of
    FormSuccess reply -> do
      _ <- runDB $ insert reply
      setMessage $ toHtml $ (replyTitle reply) <> " replied"
      redirect $ ThreadR threadId
    _ -> defaultLayout $ do
      setTitle "Please correct your comment form"
      $(widgetFile "replyAddError")

replyForm :: Form Reply
replyForm = renderDivs $ Reply
    <$> areq textField "Title" Nothing
    <*> areq textField "Author" Nothing
    <*> areq nicHtmlField "Content" Nothing

