{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Handler where

import qualified Web.Scotty as S
import Web.Scotty.Cookie

import Data.Monoid ((<>))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Applicative

import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import Database.MySQL.Simple as Db

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

import BasicAuth
import Template


-- главная страница со списком товаров
indexHandler conn = S.get "/" $ withAuth conn $ \user -> do
  items <- liftIO $ query_ conn "select id,name,count,price from Items"
  outHtml $ baseTemplate "Главная" $ do
    headerTemplate user
    table $ do
      tr $ do
          let thw (w::Int) = th ! width (toValue w) ! A.style "text-align:right"
          thw 200 "Наименование"
          thw 100 "Цена"
          thw 150 "Количество"
          when (isAdmin user) $ thw 100 "" >> thw 100 ""
      forM_ items $ \(id'::Int,name::T.Text,count::Double,price::Double) -> do
        tr $ do
          let rtd = td ! A.style "text-align:right"
          rtd $ toHtml name
          rtd $ toHtml price <> " р."
          rtd $ toHtml count
          when (isAdmin user) $ do
            rtd $ a ! href ("/remove/" <> toValue id') $ "Удалить"
            rtd $ a ! href ("/edit/" <> toValue id') $ "Редактировать"
    when (isAdmin user) $ do
      p $ a ! href "/edit/0" $ "Добавить новый товар"

-- удаление товара по id
removeHandler conn = S.get "/remove/:id" $ withAuth conn $ \user -> do
  when (isAdmin user) $ do
    id' <- (S.param "id") `S.rescue` (\_ -> return 0)
    liftIO $ execute conn "delete from Items where id=?" (Only (id'::Int))
    S.redirect "/"

-- сохранение или создание нового товара
saveHandler conn = S.post "/save/:id" $ withAuth conn $ \user -> do
  when (isAdmin user) $ do
    id' <- (S.param "id") `S.rescue` (\_ -> return 0)
    name <- S.param "name"
    count <- S.param "count"
    price <- S.param "price"
    case id' of
      0 -> liftIO $ execute conn 
                            "insert into Items (name,count,price) values (?,?,?)"
                            (name::T.Text,count::Double,price::Double)
      _ -> liftIO $ execute conn 
                            "update Items set name=?,count=?,price=? where id=?"
                            (name::T.Text,count::Double,price::Double,id'::Int)
    S.redirect "/"

-- страница редактирования товара
editHandler conn = S.get "/edit/:id" $ withAuth conn $ \user -> do
  when (isAdmin user) $ do
    (id'::Int) <- (S.param "id") `S.rescue` (\_ -> return 0)
    [(name::T.Text, count::Double, price::Double)] <- case id' of
        0 -> return [("", 0, 0)]
        _ -> liftIO $ query conn "select name,count,price from Items where id=?" (Only id')
    
    outHtml $ baseTemplate (if id' == 0 then "Новый товар" else toHtml name) $ do
      headerTemplate user
      H.form ! action ("/save/" <> toValue id') ! method "post" $ do
        table $ do
          tr $ do
            td "Наименование"
            td $ input ! A.name "name" ! type_ "text" ! value (toValue name)
          tr $ do
            td "Цена"
            td $ input ! A.name "price" ! type_ "text" ! value (toValue price)
          tr $ do
            td "Количество"
            td $ input ! A.name "count" ! type_ "text" ! value (toValue count)
          tr $ do
            td ""
            td $ do
              input ! type_ "submit" ! value "Сохранить"
              H.span " "
              a ! href "/" ! A.style "margin: 5px" $ "Отмена"

getLoginHandler conn = S.get "/login" $ do
  outHtml $ baseTemplate "Вход на сайт" $ do
    H.form ! action "/login" ! method "post" $ do
      table $ do
        tr $ do
          td "Логин"
          td $ input ! A.name "login" ! type_ "text"
        tr $ do
          td "Пароль"
          td $ input ! A.name "password" ! type_ "password"
        tr $ do
          td ""
          td $ input ! type_ "submit" ! value "Войти"

postLoginHandler conn = S.post "/login" $ do
  let getParam key = (Just <$> S.param key) `S.rescue` (\_ -> return Nothing)
  logpass <- (,) <$> getParam "login" <*> getParam "password"
  case logpass of
    (Just login, Just password) -> do
      mDbPassword <- liftIO $ query conn "select password from Users where name=?" (Only login)
      case mDbPassword of
        [Only dbPassword] | eqMd5 dbPassword password -> do
            S.addHeader "Set-Cookie" ("login=" <> TL.fromStrict login)
            S.addHeader "Set-Cookie" ("password=" <> TL.fromStrict password)
            S.redirect "/"
        _ -> S.redirect "/login"
    _ -> S.redirect "/login"

logoutHandler conn = S.get "/logout" $ do
  deleteCookie "login"
  deleteCookie "password"
  S.redirect "/"
