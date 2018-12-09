{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Dicas where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql

widgetFooter :: Widget
widgetFooter = $(whamletFile "templates/footer.hamlet")

widgetMenu :: Widget
widgetMenu = do
    sess <- lookupSession "_USR"
    mUserId <- lookupSession "_ID"    
    $(whamletFile "templates/menu.hamlet")


getDicasR :: Handler Html
getDicasR = do 
    (widget, enctype) <- generateFormPost formDicas
    nomeDica <- runDB $ selectList [] [Asc ArtigoNome]
    foto <- mapM (\(Entity artigoid _) -> runDB $ selectFirst [PassosArtigoid ==. artigoid] []) nomeDica
    let x = zip nomeDica foto
    defaultLayout $ do
        $(whamletFile "templates/dicas.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")
        toWidget $(luciusFile "templates/dicas.lucius")
        addStylesheet $ (StaticR css_bootstrap_css)

formDicas :: Form (Maybe (CategoriaId), Maybe (Text))
formDicas = renderBootstrap $  pure (,)        
    <*> aopt  (selectField $ optionsPersistKey [] [] categoriaNome) "Categoria:" Nothing   
    <*> aopt  textField "Nome:" Nothing    
    
        
getPesq2R :: Handler Html
getPesq2R = do 
    ((res,_),_) <- runFormPost formDicas
    case res of
        FormSuccess (cat, nome) -> do 
            case nome of 
                Just name -> do            
                 nomeDica <- runDB $ selectList [ArtigoNome ==. name][ Asc ArtigoNome]
                 foto <- mapM (\(Entity artigoid _) -> runDB $ selectFirst [PassosArtigoid ==. artigoid] []) nomeDica
                 let x = zip nomeDica foto
                 defaultLayout $ do
                     $(whamletFile "templates/dicas.hamlet")
                     toWidget $(luciusFile "templates/menu.lucius")
                     toWidget $(luciusFile "templates/footer.lucius")
                     toWidget $(luciusFile "templates/dicas.lucius")
                     addStylesheet $ (StaticR css_bootstrap_css)
                Nothing -> do
                 case cat of 
                     Just catego -> do            
                      nomeDica <- runDB $ selectList [ArtigoCategoriaid ==. catego][ Asc ArtigoCategoriaid]
                      foto <- mapM (\(Entity artigoid _) -> runDB $ selectFirst [PassosArtigoid ==. artigoid] []) nomeDica
                      let x = zip nomeDica foto
                      defaultLayout $ do
                          $(whamletFile "templates/dicas.hamlet")
                          toWidget $(luciusFile "templates/menu.lucius")
                          toWidget $(luciusFile "templates/footer.lucius")   
                          toWidget $(luciusFile "templates/dicas.lucius")
                          addStylesheet $ (StaticR css_bootstrap_css)
                     Nothing -> do
                          setMessage [shamlet|
                              <div class="alert alert-danger">
                                  Não temos informacoes para esta pesquisa
                          |]
                          redirect $ UsuarioR 
 