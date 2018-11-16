{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Dicas where

import Import
import Text.Lucius
import Text.Julius


widgetMenu :: Widget
widgetMenu = $(whamletFile "templates/menu.hamlet")


getDicasR :: Handler Html
getDicasR = do 
    defaultLayout $ do
        $(whamletFile "templates/dicas.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")