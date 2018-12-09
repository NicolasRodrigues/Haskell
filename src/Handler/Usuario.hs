{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql
import Control.Monad.Zip
import Yesod.Form
import Handler.Formulario

getExibirUsuarioR :: Handler Html
getExibirUsuarioR = do 
    sess <- lookupSession "_USR"
 
    usu <- runDB $ selectList [][ Asc UsuarioId]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)        
        $(whamletFile "templates/exibirusuario.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")   

getAlterarSenhaR :: UsuarioId -> Handler Html
getAlterarSenhaR aid = do 
    (widget, enctype) <- generateFormPost formSenha
    msg <- getMessage
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/alterarsenha.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")

postAlterarSenhaR ::UsuarioId -> Handler Html
postAlterarSenhaR aid = do 
    [Entity iid info] <- runDB $ selectList [UsuarioId ==. aid][]
    ((res,_),_) <- runFormPost formSenha
    case res of
        FormSuccess (s1,s2,s3) -> do
            if (usuarioSenha info) == s1 then do
                if s2 == s3 then do
                    runDB $ update iid [UsuarioSenha =. s2] 
                    setMessage [shamlet|
                        <div class="alert alert-danger">
                             Senha Alterada.
                    |]
                    redirect UsuarioR
                else do 
                    setMessage [shamlet|
                        <div class="alert alert-danger">
                             Nova senha não confere! Digite novamente.
                    |]
                    redirect UsuarioR
            else do
                setMessage [shamlet|
                    <div class="alert alert-danger">
                         Senha invalida! Digite novamente.
                |]
                redirect UsuarioR            

formSenha :: Form (Text, Text, Text)
formSenha = renderBootstrap $  pure (,,)
    <*> areq passwordField "Password Anterior: " Nothing
    <*> areq passwordField "Password Novo: " Nothing
    <*> areq passwordField "Password Confirmation: " Nothing

formUsuario :: Form (Usuario, Text)
formUsuario = renderBootstrap $  pure (,)
    <*> (Usuario 
        <$> areq textField "Nome: " Nothing
        <*> areq emailField "E-mail: " Nothing 
        <*> areq passwordField "Password: " Nothing
    )
    <*> areq passwordField "Password Confirmation: " Nothing
                        

postUsuarioR :: Handler Html
postUsuarioR = do 
    ((res,_),_) <- runFormPost formUsuario
    case res of
        FormSuccess (usr, passwordC) -> do 
            if (usuarioSenha usr) == passwordC then do
                runDB $ insert usr 
                setMessage [shamlet|
                    <div class="alert alert-success">
                        Usuario cadastrado!
                |]
                redirect HomeR
            else do 
                setMessage [shamlet|
                    <div class="alert alert-danger">
                        Senhas não conferem! Digite novamente.
                |]
                redirect UsuarioR

widgetFooter :: Widget
widgetFooter = $(whamletFile "templates/footer.hamlet")

widgetMenu :: Widget
widgetMenu = do
    sess <- lookupSession "_USR"
    $(whamletFile "templates/menu.hamlet")

getUsuarioR :: Handler Html
getUsuarioR = do 
    (widgetUsu, enctype) <- generateFormPost formUsuario
    msg <- getMessage
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/usuario.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")
        
-- deletar a categoria de acordo com o categoriaId recebido
getApagarUsuarioR :: UsuarioId ->  Handler Html
getApagarUsuarioR aid = do  
    runDB $ deleteCascade  aid
    redirect HomeR            