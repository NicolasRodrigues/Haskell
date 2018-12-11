{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Teste where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql
import Control.Monad.Zip
import Yesod.Form
import Handler.Formulario

        
--
-- ============ USUARIO ============
--
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
    


--
-- ============ CATEGORIA ============
--
-- insert Categoria
-- curl -X POST https://projhaskell-danfatec13.c9users.io/categoria/teste2 -d '{"nome":"Programação"}'	
-- delete
-- curl -X DELETE https://projhaskell-danfatec13.c9users.io/categoria/teste4/1/apagar
-- select
-- curl -v GET https://projhaskell-danfatec13.c9users.io/categoria/teste
-- update
-- curl -X PUT https://projhaskell-danfatec13.c9users.io/categoria/teste5/1/alterar  -d '{"nome":"Haskell"}'	



-- update from categoria 
-- set (todos os campos)
-- where categoria.id = categoriaid
putAlterarCategoriaR :: CategoriaId -> Handler TypedContent
putAlterarCategoriaR categoriaid = do 
    _ <- runDB $ get404 categoriaid
    altCategoria <- requireJsonBody :: Handler Categoria
    runDB $ replace categoriaid altCategoria 
    sendStatusJSON noContent204 (object [])
    


    


        


formArtigo :: Day -> Form Artigo
formArtigo x2 = renderBootstrap $ (Artigo 
        <$> areq textField "Nome: " Nothing
        <*> areq (selectField listaCategoriaq) FieldSettings{fsId=Just "li",
                           fsLabel="Categoria :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Policarpo Quaresma")]} Nothing        
        <*> pure x2 
        <*> pure 0
        <*> pure 0 
        <*> pure 0
        <*> pure 0        
        
    )
    
listaCategoriaq = do
       entidades <- runDB $ selectList [] [Asc CategoriaNome] 
       optionsPairs $ fmap (\ent -> (categoriaNome $ entityVal ent, entityKey ent)) entidades    
       
      
diaHj :: IO Day
diaHj = fmap utctDay getCurrentTime 


                    
-- para trazer todos as categorias .
getListArtigosR :: Handler TypedContent 
getListArtigosR = do 
    artigos <- runDB $ selectList [] [Asc ArtigoNome]
    sendStatusJSON ok200 (object ["resp" .= artigos]) 
     
    
--formPasso :: Form Passos
---formPasso = renderBootstrap $ (Passos
 ---       <$> pure 
---        <*> areq textField "Título: " Nothing
  ---      <*> pure "entrar o caminhodaFoto" 
--        <*> areq textareaField FieldSettings{fsId=Just "campo4",
 --                          fsLabel="Descricao",
  --                         fsTooltip= Nothing,
--                           fsName= Nothing,
 --                          fsAttrs=[("class","form-control"),("placeholder","EX: Em uma terra muito muito distante..."),("style","display:inline-block")]} Nothing
  --  )  