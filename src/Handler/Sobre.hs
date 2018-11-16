{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Sobre where

import Import
import Text.Lucius
import Text.Julius



widgetMenu :: Widget
widgetMenu = $(whamletFile "templates/menu.hamlet")


getSobreR :: Handler Html
getSobreR = do 
    defaultLayout $ do
        $(whamletFile "templates/sobre.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")