{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Cadastro where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

{-* formCadastro :: Form Cadastro
formCadastro = do
    <$> areq textField "Nome da dica: " Nothing
    <*> areq intField "Descrição da dica: " Nothing
    <*> areq textField "1ºPasso: " Nothing
    <*> areq textField "2ºPasso: " Nothing
    <*> areq textField "3ºPasso: " Nothing
-}
   
widgetFooter :: Widget
widgetFooter = $(whamletFile "templates/footer.hamlet")

widgetMenu :: Widget
widgetMenu = $(whamletFile "templates/menu.hamlet")


getCadastroR :: Handler Html
getCadastroR = do 
    defaultLayout $ do
        $(whamletFile "templates/cadastro.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")
        toWidget $(luciusFile "templates/cadastro.lucius")
        