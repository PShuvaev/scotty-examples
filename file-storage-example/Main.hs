{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
import qualified Web.Scotty as S
import Network.Wai.Parse

import Control.Monad.IO.Class
import Control.Applicative
import Control.Monad
import Data.Monoid

import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import System.Directory
import System.Posix
import System.FilePath.Posix
import Data.Time.Format
import Data.Time.Clock.POSIX
import System.Locale

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding
import qualified Data.Text as T

storageDir = "storage"
mkPath filename = storageDir <> "/" <> filename

formatModTime = formatTime defaultTimeLocale "%Y/%m/%d %T" . posixSecondsToUTCTime . realToFrac
outHtml = S.html . renderHtml

-- базовый шаблон страниц
baseTemplate title' body' = docTypeHtml $ do
  H.head $ do
    H.title title'
    meta ! charset "utf-8"
  body body'

isImage name = snd (splitExtension name) `elem` [".png", ".jpg", ".jpeg", ".png"]

indexHandler = S.get "/" $ do
  let realFile x = not $ elem x [".", ".."]
  files <- liftIO $ (filter realFile) <$> getDirectoryContents storageDir
  filesInfo <- liftIO $ mapM (getFileStatus . mkPath) files
  
  outHtml $ baseTemplate "Index" $ do
    H.style "table{border-collapse: collapse;} th,td{border: 1px solid black; width: 200px;} img{width:150px;}"
    table $ do
      tr $ do
        th "Имя файла"
        th "Изменен"
        th "Размер"
        th ""
      forM_ (zip files filesInfo) $ \(file, info) -> do
        tr $ do
          td $ do
            H.div $ a ! href (toValue $ mkPath file) $ toHtml file
            when (isImage file) (img ! src (toValue $ mkPath file))
          td $ toHtml $ formatModTime $ modificationTime info
          td $ toHtml $ (show $ fileSize info) <> " bytes"
          td $ do
            a ! href (toValue $ "/remove/"<>file) $ "Удалить"
            br
            a ! href (toValue $ "/rename/"<>file) $ "Переименовать"
    
    H.form ! action "/upload" ! method "post" ! enctype "multipart/form-data" $ do
      H.label $ do
        input ! type_ "file" ! name "file"
        input ! type_ "submit" ! value "Загрузить файл"

uploadHandler = S.post "/upload" $ do
  [(_,  FileInfo fileName _ fileContent)] <- S.files
  let decodedFileName = T.unpack $ decodeUtf8 fileName
  liftIO $ LBS.writeFile (mkPath decodedFileName) fileContent
  S.redirect "/"

fileHandler = S.get "/storage/:file" $ do
  filename <- S.param "file" 
  S.file (mkPath filename)

removeHandler = S.get "/remove/:file" $ do
  filename <- S.param "file"
  liftIO $ removeFile $ mkPath filename
  S.redirect "/"

getRenameHandler = S.get "/rename/:file" $ do
  (file::String) <- S.param "file"
  outHtml $ baseTemplate "Rename" $ do
    H.form ! action (toValue $ "/rename/"<>file) ! method "post" $ do
      input ! type_ "text" ! name "newName" ! value (toValue $ file)
      input ! type_ "submit" ! value "Переименовать"

postRenameHandler = S.post "/rename/:oldName" $ do
  oldName <- S.param "oldName"
  newName <- S.param "newName"
  liftIO $ renameFile (mkPath oldName) (mkPath newName)
  S.redirect "/"

main = do
  S.scotty 3000 $ do
    indexHandler
    fileHandler
    removeHandler
    getRenameHandler
    postRenameHandler
    uploadHandler
