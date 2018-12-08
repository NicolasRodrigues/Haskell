{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Login where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql


widgetFooter :: Widget
widgetFooter = $(whamletFile "templates/footer.hamlet")

widgetMenu :: Widget
widgetMenu = do
    sess <- lookupSession "_USR"
    $(whamletFile "templates/menu.hamlet")


formLogin :: Form (Text, Text)
formLogin = renderBootstrap $ pure (,)
    <*> areq emailField "E-mail: " Nothing
    <*> areq passwordField "Password: " Nothing

getLoginR :: Handler Html
getLoginR = do 
    (widgetLogin, enctype) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/login.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")
        toWidget $(luciusFile "templates/cadastro.lucius")
        toWidget $(luciusFile "templates/login.lucius")

postLoginR :: Handler Html
postLoginR = do 
    ((res,_),_) <- runFormPost formLogin
    case res of 
        FormSuccess ("admin@admin.com","admin") -> do
            setSession "_USR" "admin"
           -- redirect AdminR
            redirect HomeR
        FormSuccess (email,senha) -> do 
            usr <- runDB $ selectFirst [UsuarioEmail ==. email
                                       ,UsuarioSenha ==. senha] []
            case usr of 
                Just (Entity usrid usuario) -> do 
                    setSession "_USR" (pack (show usuario))
                    redirect HomeR
                Nothing -> do 
                    setMessage [shamlet|
                            Usuario não encontrado
                    |]
                    redirect LoginR
        _ -> redirect LoginR

getLogoutR :: Handler Html
getLogoutR = do 
    deleteSession "_USR"
    redirect HomeR        