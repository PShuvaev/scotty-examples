{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module BasicAuth where

import qualified Web.Scotty as S
import Web.Scotty.Cookie

import Data.Monoid ((<>))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative

import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import Network.HTTP.Types.Status
import Database.MySQL.Simple as Db

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TEnc

import Data.Hash.MD5

import Template

type User = T.Text

textToBs = TL.foldl' BS8.snoc BS8.empty

decodeAuth :: TL.Text -> (T.Text, T.Text)
decodeAuth auth = 
  let 
    ("Basic", _encodedStr) = TL.breakOn " " auth
    (Right decodedAuthBS) = Base64.decode $ textToBs (TL.tail _encodedStr)
    (login, colonAndPassword) = T.breakOn ":" (TEnc.decodeUtf8 decodedAuthBS)
    password = T.tail colonAndPassword
  in (login, password)

withBasicAuth :: Db.Connection -> (User -> S.ActionM ()) -> S.ActionM ()
withBasicAuth conn handlerProc = do  
  let requestAuth = do
        S.status unauthorized401
        S.addHeader "WWW-Authenticate" "Basic realm=\"Secure realm\""
  
  mauth <- S.header "Authorization"
  case mauth of
    Nothing -> requestAuth
    Just auth -> do
      let (login, password) = decodeAuth auth
      mDbPassword <- liftIO $ query conn "select password from Users where name=?" (Only login)
      case mDbPassword of
        [Only dbPassword] | eqMd5 dbPassword password
          -> handlerProc login
        _ -> do
          requestAuth
          outHtml $ baseTemplate "Аутентификация" $ do
            H.div "Для просмотра сайта необходима аутентификация."
            H.div $ a ! href "/" $ "Войти"

withCookieAuth :: Db.Connection -> (User -> S.ActionM ()) -> S.ActionM ()
withCookieAuth conn handlerProc = do
  let requestAuth = S.redirect "/login"
  
  logpass <- (,) <$> getCookie "login" <*> getCookie "password"
  case logpass of
    (Just login, Just password) -> do
      mDbPassword <- liftIO $ query conn "select password from Users where name=?" (Only login)
      case mDbPassword of
        [Only dbPassword] | eqMd5 dbPassword password
          -> handlerProc login
        _ -> requestAuth
    _ -> requestAuth

eqMd5 :: T.Text -> T.Text -> Bool
eqMd5 md5Str testStr = md5Str == (T.pack $ md5s $ Str $ T.unpack testStr)

isAdmin user = user == "admin"

withAuth = withCookieAuth
