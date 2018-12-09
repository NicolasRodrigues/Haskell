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
import Data.Maybe (fromJust)
import Database.Persist.Sql

widgetFooter :: Widget
widgetFooter = $(whamletFile "templates/footer.hamlet")

widgetMenu :: Widget
widgetMenu = do
    sess <- lookupSession "_USR"
    mUserId <- lookupSession "_ID"    
    $(whamletFile "templates/menu.hamlet")


getSobreR :: Handler Html
getSobreR = do 
    sess <- lookupSession "_USR"
    [Entity iid info] <- runDB $ selectList [][Asc SobreId]
    ((res,widget), enctype) <- runFormPost $ formSobre (sobreComoSomos info) (sobreComoFunciona info)
                                         (sobreComoPossoAjudar info) (sobreComoContribuir info)
    case res of
        FormSuccess sob -> do
            runDB $ replace iid sob
            redirect HomeR
        _ -> defaultLayout $ do 
            addStylesheet $ (StaticR css_bootstrap_css)        
            $(whamletFile "templates/cadastroempresa.hamlet")
            toWidget $(luciusFile "templates/menu.lucius")
            toWidget $(luciusFile "templates/footer.lucius")
        
formSobre ::  Textarea -> Textarea -> Textarea -> Textarea -> Form Sobre
formSobre a b c d = renderBootstrap $ (Sobre 
        <$> areq textareaField "Como Somos:" (Just a)
        <*> areq textareaField "Como Funcionamos:" (Just b)
        <*> areq textareaField "Como Podemos Ajudar:" (Just c)
        <*> areq textareaField "Como Contribuir:" (Just d)
    )        

postSobreER :: Handler Html
postSobreER = getSobreR 
            
            


getSobreExibirR :: Handler Html
getSobreExibirR = do 
    [Entity iid info] <- runDB $ selectList [][Asc SobreId]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)        
        $(whamletFile "templates/sobre.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")            