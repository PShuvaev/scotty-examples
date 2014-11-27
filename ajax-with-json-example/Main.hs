{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}
import qualified Web.Scotty as S

import Data.Monoid ((<>))
import Control.Monad (forM_,forM)
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
import Data.Text (Text)

import Data.Aeson as Aeson

outHtml = S.html . renderHtml

data Item = Item {
  itemId :: Int,
  itemName :: Text,
  itemPrice :: Double,
  itemCount :: Double
} deriving (Show)

instance FromJSON Item where
  parseJSON (Object v) = Item <$> v.:"id" <*> v.:"name" <*> v.:"price" <*> v.:"count"

instance ToJSON Item where
  toJSON (Item id' name price count) =
    Aeson.object ["id" .= id', "name" .= name, "price" .= price, "count" .= count]

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
    H.script ! src "/jquery.js" $ ""
    H.script ! src "/Main.js" $ ""
    H.style "th[data=id],td[data=id]{display:none}"
    
    H.div $ do
      H.span "Поиск по наименованию "
      input ! A.id "search" ! type_ "text"
    
    table ! A.id "itemsTable" $ do
      tr $ do
          let thw (w::Int) = th ! width (toValue w) ! A.style "text-align:right"
          th ! A.style "display:none" $ ""
          thw 200 "Наименование"
          thw 100 "Цена"
          thw 10 ""
          thw 150 "Количество"
          thw 100 "" >> thw 100 ""
      forM_ items $ \(id'::Int,name::T.Text,count::Double,price::Double) -> do
        tr ! A.data_ (toValue id') $ do
          let rtd = td ! A.style "text-align:right"
          td ! A.data_ "id" $ toHtml id'
          rtd ! A.data_ "name" $ toHtml name
          rtd ! A.data_ "price" $ toHtml price
          rtd $ " р."
          rtd ! A.data_ "count" $ toHtml count
          rtd $ a ! class_ "removeLink" $ "Удалить"
          rtd $ a ! class_ "editLink" $ "Редактировать"
    H.br
    
    table ! A.id "editProductTable" $ do
      tr $ do
        td ""
        td $ input ! A.name "id" ! type_ "hidden"
      tr $ do
        td "Наименование"
        td $ input ! A.name "name" ! type_ "text"
      tr $ do
        td "Цена"
        td $ input ! A.name "price" ! type_ "text"
      tr $ do
        td "Количество"
        td $ input ! A.name "count" ! type_ "text"
      tr $ do
        td ""
        td $ do
          input ! type_ "submit" ! A.id "saveBtn" ! value "Добавить"
          H.span " "
          input ! type_ "submit" ! A.id "resetBtn" ! value "Сбросить"

-- удаление товара по id
removeHandler conn = S.post "/remove/:id" $ do
  id' <- (S.param "id") `S.rescue` (\_ -> return 0)
  liftIO $ execute conn "delete from Items where id=?" (Only (id'::Int))
  S.json ["result" .= True]

-- сохранение или создание нового товара
saveHandler conn = S.post "/save/:id" $ do
  id' <- (S.param "id") `S.rescue` (\_ -> return 0)
  Just item <- Aeson.decode <$> S.body
  
  let name = itemName item; count = itemCount item; price = itemPrice item
  
  case id' of
    0 -> do
      [Only (countItemsWithSameName::Integer)] <- liftIO $ query conn "select count(id) from Items where name=?" (Only name)
      if countItemsWithSameName > 0
        then S.json $ Aeson.object ["result" .= False]
        else do
          newId <- liftIO $ do
                      execute conn "insert into Items (name,count,price) values (?,?,?)" (name,count,price)
                      Db.insertID conn
          S.json $ Aeson.object ["result" .= True, "id" .= newId]
    _ -> do
      liftIO $ execute conn "update Items set name=?,count=?,price=? where id=?" (name,count,price,id')
      S.json $ Aeson.object ["result" .= True, "id" .= (id'::Int)]


searchHandler conn = S.get "/search/:query" $ do
  (name::T.Text) <- S.param "query"
  items <- liftIO $ query conn "select id,name,count,price from Items where name like concat('%', ?, '%') collate utf8_general_ci" (Only name)     
  typedItems <- forM items $ \(id',name,count,price) -> (return $ Item id' name price count)
  S.json $ Aeson.object ["result" .= True, "rows" .= typedItems]
  
main = do
  conn <- connect defaultConnectInfo {  
                                        connectUser = "root",
                                        connectPassword = "root",
                                        connectDatabase = "lab"
                                      }
  S.scotty 3000 $ do
    indexHandler conn
    saveHandler conn
    removeHandler conn
    searchHandler conn
    S.get "/Main.js" (S.file "Main.js")
    S.get "/jquery.js" (S.file "jquery.js")