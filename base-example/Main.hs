{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import qualified Web.Scotty as S

import Data.Monoid ((<>))
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative

import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import Database.MySQL.Simple as Db

import qualified Data.Text as T

outHtml = S.html . renderHtml

-- базовый шаблон страниц
baseTemplate title' body' = docTypeHtml $ do
  H.head $ do
    H.title title'
    meta ! charset "utf-8"
  body body'

-- главная страница со списком товаров
indexHandler conn = S.get "/" $ do
  items <- liftIO $ query_ conn "select id,name,count,price from Items"
  outHtml $ baseTemplate "Главная" $ do
    table $ do
      tr $ do
          let thw (w::Int) = th ! width (toValue w) ! A.style "text-align:right"
          thw 200 "Наименование"
          thw 100 "Цена"
          thw 150 "Количество"
          thw 100 "" >> thw 100 ""
      forM_ items $ \(id'::Int,name::T.Text,count::Double,price::Double) -> do
        tr $ do
          let rtd = td ! A.style "text-align:right"
          rtd $ toHtml name
          rtd $ toHtml price <> " р."
          rtd $ toHtml count
          rtd $ a ! href ("/remove/" <> toValue id') $ "Удалить"
          rtd $ a ! href ("/edit/" <> toValue id') $ "Редактировать"
    p $ a ! href "/edit/0" $ "Добавить новый товар"

-- удаление товара по id
removeHandler conn = S.get "/remove/:id" $ do
  id' <- (S.param "id") `S.rescue` (\_ -> return 0)
  liftIO $ execute conn "delete from Items where id=?" (Only (id'::Int))
  S.redirect "/"

-- сохранение или создание нового товара
saveHandler conn = S.post "/save/:id" $ do
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
editHandler conn = S.get "/edit/:id" $ do
  (id'::Int) <- (S.param "id") `S.rescue` (\_ -> return 0)
  [(name::T.Text, count::Double, price::Double)] <- case id' of
      0 -> return [("", 0, 0)]
      _ -> liftIO $ query conn "select name,count,price from Items where id=?" (Only id')
  
  outHtml $ baseTemplate (if id' == 0 then "Новый товар" else toHtml name) $ do
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

main = do
  conn <- connect defaultConnectInfo {  
                                        connectUser = "root",
                                        connectPassword = "root",
                                        connectDatabase = "lab"
                                      }
  S.scotty 3000 $ mapM_($ conn)[indexHandler,saveHandler,editHandler,removeHandler]
