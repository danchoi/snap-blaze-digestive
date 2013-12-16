{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Data.Maybe (isJust)
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Control.Monad.IO.Class
import Data.Text (Text)
import Text.Blaze ((!))
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H
-- import qualified Text.Blaze.Html5.Attributes as A

import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Digestive.Snap
import Text.Digestive.Util
import Snap.Blaze

data User = User
  { userName :: Text
  , userMail :: Text
  } deriving (Show)

userForm :: Monad m => Form Text m User
userForm = User 
  <$> "name" .: check "Can't be empty" (not . T.null) (text  Nothing)
  <*> "mail" .: check "Not a valid email address" checkEmail (text Nothing)

checkEmail :: Text -> Bool
checkEmail = isJust . T.find (== '@')

userView :: View H.Html -> H.Html
userView view = do
  errorList "name" view
  label "name" view "Name: " 
  inputText "name" view
  H.br
  errorList "mail" view
  label "mail" view "Email address: "
  inputText "mail" view
  H.br

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (formHandler) 

template :: H.Html -> H.Html
template body = H.docTypeHtml $ do
  H.head $ do
    H.title "snap digestive functors blaze test"
  H.body body

formHandler :: Snap ()
formHandler = do
    r <- runForm "userform" userForm
    case r of 
      (view, Nothing) -> do 
        liftIO $ putStrLn "rendering form"
        let view' = fmap H.toHtml view
        blaze $ template $ 
          form view' "/" $ do
            userView view'
            H.br
            inputSubmit "Submit"
      (_, Just user) -> 
        writeText $ T.pack $ show user

