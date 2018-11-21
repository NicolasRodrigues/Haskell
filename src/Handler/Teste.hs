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
                    <h1>
                        Usuario cadastrado!
                |]
                redirect HomeR
            else do 
                setMessage [shamlet|
                    <h1>
                        Senhas não conferem
                |]
                redirect UsuarioR

getUsuarioR :: Handler Html
getUsuarioR = do 
    (widgetUsu, enctype) <- generateFormPost formUsuario
    msg <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/usuario.hamlet")
        
        
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

	
-- para trazer todos as categorias .
getListCategoriaR :: Handler TypedContent
getListCategoriaR = do 
    categorias <- runDB $ selectList [] [Asc CategoriaNome]
    sendStatusJSON ok200 (object ["resp" .= categorias])
    
-- para inserir nova categoria.
postInsereCategoriaR :: Handler TypedContent
postInsereCategoriaR = do
    categoria <- requireJsonBody :: Handler Categoria
    categoriaid <- runDB $ insert categoria
    sendStatusJSON created201 (object ["resp" .= categoriaid])
    
-- deletar a categoria de acordo com o categoriaId recebido
deleteApagarCategoriaR :: CategoriaId -> Handler TypedContent
deleteApagarCategoriaR categoriaid = do  
    _ <- runDB $ get404 categoriaid
    runDB $ delete categoriaid
    sendStatusJSON noContent204 (object [])
    

-- update from categoria 
-- set (todos os campos)
-- where categoria.id = categoriaid
putAlterarCategoriaR :: CategoriaId -> Handler TypedContent
putAlterarCategoriaR categoriaid = do 
    _ <- runDB $ get404 categoriaid
    altCategoria <- requireJsonBody :: Handler Categoria
    runDB $ replace categoriaid altCategoria 
    sendStatusJSON noContent204 (object [])
    

formCategoria :: Form Categoria
formCategoria = renderBootstrap $   (Categoria 
        <$> areq textField "Nome: " Nothing
    )
    

postCategoriaR :: Handler Html
postCategoriaR = do 
    ((res,_),_) <- runFormPost formCategoria
    case res of
        FormSuccess (cat) -> do 
                runDB $ insert cat 
                setMessage [shamlet|
                    <h1>
                        Categoria cadastrado!
                |]
                redirect HomeR

getCategoriaR :: Handler Html
getCategoriaR = do 
    (widgetUsu, enctype) <- generateFormPost formCategoria
    msg <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/categoria.hamlet")
        


formArtigo :: Day -> Form Artigo
formArtigo x2 = renderBootstrap $ (Artigo 
        <$> areq (selectField listaCategoria) FieldSettings{fsId=Just "li",
                           fsLabel="Categoria :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Policarpo Quaresma")]} Nothing
                           
        <*> areq textField "Nome: " Nothing
        <*> pure x2 
        
    )
    
listaCategoria = do
       entidades <- runDB $ selectList [] [Asc CategoriaNome] 
       optionsPairs $ fmap (\ent -> (categoriaNome $ entityVal ent, entityKey ent)) entidades    
       
      
diaHj :: IO Day
diaHj = fmap utctDay getCurrentTime 

getArtigoR :: Handler Html
getArtigoR = do 
    diaMat <- liftIO diaHj
    (widgetArt, enctype) <- generateFormPost  (formArtigo diaMat)
    msg <- getMessage
    defaultLayout $ do 
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/artigo.hamlet")
        
postArtigoR :: Handler Html
postArtigoR = do 
    diaMat <- liftIO diaHj
    ((res,_),_) <- runFormPost (formArtigo diaMat)
    case res of
        FormSuccess (art) -> do 
                artigoid <- runDB $ insert art 
                rumDB $ insert  $ Utilidades artigoid 0 0 0 0 
                setMessage [shamlet|
                    <h1>
                        Artigo cadastrado!
                |]
                redirect HomeR         
                
-- deletar a categoria de acordo com o categoriaId recebido
deleteApagarArtigoR :: ArtigoId -> Handler TypedContent
deleteApagarArtigoR artigoid = do  
    _ <- runDB $ get404 artigoid
    runDB $ delete artigoid
    sendStatusJSON noContent204 (object [])
                    
-- para trazer todos as categorias .
getListArtigosR :: Handler TypedContent
getListArtigosR = do 
    artigos <- runDB $ selectList [] [Asc ArtigoNome]
    sendStatusJSON ok200 (object ["resp" .= artigos])                    