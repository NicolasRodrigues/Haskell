{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Handler.Teste where

import Import
import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

--curl -v GET https://projhaskell-danfatec13.c9users.io/teste
-- para trazer todos os usuarios.
getListUsuarioR :: Handler TypedContent
getListUsuarioR = do 
    usuarios <- runDB $ selectList [] [Asc UsuarioNome]
    sendStatusJSON ok200 (object ["resp" .= usuarios])
    
-- curl -X POST https://projhaskell-danfatec13.c9users.io/teste2 -d '{"nome":"Danilo", "email":"danilo@gmaixl.com","senha":"23"}'		
-- para inserir novo usuario.
postInsereUsuarioR :: Handler TypedContent
postInsereUsuarioR = do
    usuario <- requireJsonBody :: Handler Usuario
    usuarioid <- runDB $ insert usuario
    sendStatusJSON created201 (object ["resp" .= usuarioid])
    
-- curl -X DELETE https://projhaskell-danfatec13.c9users.io/teste4/1/apagar
deleteApagarR :: UsuarioId -> Handler TypedContent
deleteApagarR usrid = do  
    _ <- runDB $ get404 usrid
    runDB $ delete usrid
    sendStatusJSON noContent204 (object [])
    
-- curl -X PUT https://projhaskell-danfatec13.c9users.io/teste5/21/alterar  -d '{"nome":"Alberto", "email":"alberto@alberto.com","senha":"63500"}'	    
-- update from usuario 
-- set (todos os campos)
-- where usuario.id = usuarioid
putAlterarR :: UsuarioId -> Handler TypedContent
putAlterarR usuarioid = do 
    _ <- runDB $ get404 usuarioid
    altUsuario <- requireJsonBody :: Handler Usuario 
    runDB $ replace usuarioid altUsuario 
    sendStatusJSON noContent204 (object [])
    

