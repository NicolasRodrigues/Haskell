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
import Text.Show(show)
import Database.Persist.Sql



widgetMenu :: Widget
widgetMenu = do
    sess <- lookupSession "_USR"
    $(whamletFile "templates/menu.hamlet")

widgetFooter :: Widget
widgetFooter = $(whamletFile "templates/footer.hamlet")

getHomeR :: Handler Html
getHomeR = do 
    sess <- lookupSession "_USR"
    nomeDica <- runDB $ selectList [] [Asc ArtigoNome]
    foto <- mapM (\(Entity artigoid _) -> runDB $ selectFirst [PassosArtigoid ==. artigoid] []) nomeDica
    let x = zip nomeDica foto    
    putStrLn $ pack $ "============================================"
    putStrLn $ (pack.show) $ length x
    putStrLn $ pack $ "============================================"
    mapM_ (\ v -> putStrLn $ (pack.show) v) foto
    defaultLayout $ do
        $(whamletFile "templates/home.hamlet")
        toWidgetHead $(juliusFile "templates/home.julius")
        toWidget $(luciusFile "templates/home.lucius")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")

