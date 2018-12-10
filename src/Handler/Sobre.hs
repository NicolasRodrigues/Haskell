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
import Yesod.Form.Bootstrap3(bfs)

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
            setTitle "Sobre"
            addStylesheet $ (StaticR css_bootstrap_css)        
            $(whamletFile "templates/cadastroempresa.hamlet")
            toWidget $(luciusFile "templates/usuario.lucius")
            toWidget $(luciusFile "templates/menu.lucius")
            toWidget $(luciusFile "templates/footer.lucius")
            
        
formSobre ::  Textarea -> Textarea -> Textarea -> Textarea -> Form Sobre
formSobre a b c d = renderBootstrap $ (Sobre 
        <$> areq textareaField (bfs ("Quem Somos:" :: Text)) (Just a)
        <*> areq textareaField (bfs ("Como Funcionamos:" :: Text)) (Just b)
        <*> areq textareaField (bfs ("Como Podemos Ajudar:" :: Text)) (Just c)
        <*> areq textareaField (bfs ("Como Contribuir:":: Text)) (Just d)
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