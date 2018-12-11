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
import Yesod.Form.Bootstrap3(bfs)


widgetFooter :: Widget
widgetFooter = $(whamletFile "templates/footer.hamlet")
    

widgetMenu :: Widget
widgetMenu = do
    sess <- lookupSession "_USR"
    mUserId <- lookupSession "_ID"    
    $(whamletFile "templates/menu.hamlet")


formLogin :: Form (Text, Text)
formLogin = renderBootstrap $ pure (,)
    <*> areq emailField (bfs ("E-mail: " :: Text)) (Nothing)
    <*> areq passwordField (bfs ("Password: " :: Text)) (Nothing)

getLoginR :: Handler Html
getLoginR = do 
    (widgetLogin, enctype) <- generateFormPost formLogin
    msg <- getMessage
    defaultLayout $ do 
        setTitle "Login"
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/login.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/login.lucius")
        toWidget $(luciusFile "templates/footer.lucius")

postLoginR :: Handler Html
postLoginR = do 
    ((res,_),_) <- runFormPost formLogin
    case res of 
        FormSuccess ("admin@admin.com","admin") -> do
            setSession "_USR" "admin"
            deleteSession "_ID" 
           -- redirect AdminR
            redirect HomeR
        
        FormSuccess (email,senha) -> do 
            usr <- runDB $ selectFirst [UsuarioEmail ==. email
                                       ,UsuarioSenha ==. senha] []
            case usr of 
                Just (Entity usrid usuario) -> do 
                    setSession "_USR" (pack (show usuario))
                    setSession "_ID" (pack $ show $ fromSqlKey usrid)
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