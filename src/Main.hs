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
import qualified Text.Blaze.Html5.Attributes as A

import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Digestive.Snap
import Text.Digestive.Util (readMaybe)
import Snap.Blaze

data User = User
  { userName :: Text
  , userMail :: Text
  } deriving (Show)

type Version = [Int]

data Category = Web | Text | Math deriving (Bounded, Enum, Eq, Show)

data Package = Package Text Version Category deriving (Show)

data Release = Release User Package deriving (Show)

userForm :: Monad m => Form Text m User
userForm = User 
  <$> "name" .: check "Can't be empty" checkTextNotNull (text  Nothing)
  <*> "mail" .: check "Not a valid email address" checkEmail (text Nothing)

packageForm :: Monad m => Form Text m Package
packageForm = Package
    <$> "name" .: text Nothing
    <*> "version" .: validate validateVersion (text (Just "0.0.0.1"))
    <*> "category" .: choice categories Nothing
  where 
    categories = [(x, T.pack (show x)) | x <- [minBound .. maxBound]]

releaseForm :: Monad m => Form Text m Release
releaseForm = Release 
    <$> "author" .: userForm
    <*> "package" .: packageForm

checkEmail :: Text -> Bool
checkEmail = isJust . T.find (== '@')

checkTextNotNull :: Text -> Bool
checkTextNotNull = not . T.null

validateVersion :: Text -> Result Text Version
validateVersion = maybe (Error "Cannot parse version") Success .
  mapM (readMaybe . T.unpack) . T.split (== '.')

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

releaseView :: View H.Html -> H.Html
releaseView view = do
    H.h2 "Author"
    userView $ subView "author" view

    H.h2 "Package"
    childErrorList "package" view

    label "package.name" view "Name: "
    inputText "package.name" view
    H.br

    label "package.version" view "Version: "
    inputText "package.version" view
    H.br
    label "package.category" view "Category: "
    inputSelect "package.category" view
    H.br 

nestedRouteHandler :: Snap ()
nestedRouteHandler = do
  writeText "nested"
  ifTop (writeText "top") <|> route [("inner", innerNestedHandler)]

innerNestedHandler :: Snap ()
innerNestedHandler =
  writeText "inner nested"


main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (formHandler)  <|>
    route [ ("nested", nestedRouteHandler) ]


template :: H.Html -> H.Html
template body = H.docTypeHtml $ do
  H.head $ do
    H.title "snap digestive functors blaze test"
    css
  H.body body

css :: H.Html
css = H.style ! A.type_ "text/css" $ do
   "label {width: 130px; float: left; clear: both}"
   "ul.digestive-functors-error-list {"
   "    color: red;"
   "    list-style-type: none;"
   "    padding-left: 0px;"
   "}"

formHandler :: Snap ()
formHandler = do
    r <- runForm "releaseForm" releaseForm
    case r of 
      (view, Nothing) -> do 
        liftIO $ putStrLn "rendering form"
        let view' = fmap H.toHtml view
        blaze $ template $ 
          form view' "/" $ do
            releaseView view'
            H.br
            inputSubmit "Submit"
      (_, Just release) -> 
        writeText $ T.pack $ show release

