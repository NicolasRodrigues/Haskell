{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Text.Lucius
import Text.Julius


getHomeR :: Handler Html
getHomeR = do  
    defaultLayout $ do 
         toWidgetHead $(juliusFile "templates/home.julius")
         toWidget $(luciusFile "templates/home.lucius")
         $(whamletFile "templates/home.hamlet")
        