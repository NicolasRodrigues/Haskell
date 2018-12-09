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
    (widget, enctype) <- generateFormPost formPesquisa
    nomeDica <- runDB $ selectList [] [Asc ArtigoNome]
    foto <- mapM (\(Entity artigoid _) -> runDB $ selectFirst [PassosArtigoid ==. artigoid] []) nomeDica
    let x = zip nomeDica foto
    defaultLayout $ do
        addStylesheet $ (StaticR css_bootstrap_css)      
        $(whamletFile "templates/dicas.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")
        toWidget $(luciusFile "templates/dicas.lucius")

formDicas :: Form (Maybe (CategoriaId), Maybe (Text))
formDicas = renderBootstrap $  pure (,)        
    <*> aopt  (selectField $ optionsPersistKey [] [] categoriaNome) "Categoria:" Nothing   
    <*> aopt  textField "Nome:" Nothing    
    
         
getListaArtigoR :: Handler Html
getListaArtigoR = do 
    art <- runDB $ selectList [][ Asc ArtigoId]
    defaultLayout $ do 
        addStylesheet $ (StaticR css_bootstrap_css)        
        $(whamletFile "templates/listaartigo.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")     
        
getApagarArtigoR :: ArtigoId ->  Handler Html
getApagarArtigoR aid = do  
    runDB $ deleteCascade  aid
    redirect ListaArtigoR            
        
        
data Pesquisa = Pesquisa
    { pesquisa          :: Text
    }
    
formPesquisa :: Form Pesquisa
formPesquisa = renderBootstrap $ Pesquisa
        <$> areq textField FieldSettings{fsId=Just "campo1",
                           fsLabel="Nome",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","Digite sua pesquisa"),("style","width:70%")]} Nothing        
                           

toTexto :: Pesquisa -> Text
toTexto (Pesquisa x) = x

postPesqArtigoR :: Handler Html
postPesqArtigoR = do 
    ((res, _), _) <- runFormPost formPesquisa
    case res of
        FormSuccess pesquisar -> do 
            let nome = toTexto(pesquisar)
            redirect (BuscarArtigoR nome)
        _ -> do
            setMessage $ [shamlet| Dados invalidos! |] 
            redirect HomeR                         
        
    
getBuscarArtigoR :: Text -> Handler Html
getBuscarArtigoR art = do
    (widget, enctype) <- generateFormPost formPesquisa
    nomeDica <- runDB $ selectList ([Filter ArtigoNome (Left $ "%"++ art ++"%") (BackendSpecificFilter "ILIKE")])[Asc ArtigoNome]    
    foto <- mapM (\(Entity artigoid _) -> runDB $ selectFirst [PassosArtigoid ==. artigoid] []) nomeDica
    let x = zip nomeDica foto
    defaultLayout $ do
         $(whamletFile "templates/dicas.hamlet")
         toWidget $(luciusFile "templates/menu.lucius")
         toWidget $(luciusFile "templates/footer.lucius") 
         toWidget $(luciusFile "templates/dicas.lucius")
         addStylesheet $ (StaticR css_bootstrap_css)         