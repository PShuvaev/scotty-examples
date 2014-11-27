{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Template where

import qualified Web.Scotty as S

import Text.Blaze
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import qualified Data.Text as T

outHtml = S.html . renderHtml

-- базовый шаблон страниц
baseTemplate title' body' = docTypeHtml $ do
  H.head $ do
    H.title title'
    meta ! charset "utf-8"
  body body'

headerTemplate user = do
  H.div ! A.style "text-align: right" $ do
    H.span (toHtml user)
    H.span " "
    a ! href "/logout" $ "Выйти"
