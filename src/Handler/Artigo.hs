{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Artigo where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql
import Data.Maybe (fromJust)

-- Text -> CategoriaId -> Day -> Int -> Int -> Int -> Int ->
-- PassosId -> Text -> Textarea ->
-- PassosId -> Text -> Textarea ->
-- PassosId -> Text -> Textarea ->
-- InfoaddId -> Textarea -> Textarea -> Textarea -> ArtigoId

--One trick we’ve introduced here is using the same handler code for both the GET and POST request methods. This is enabled by the implementation of runFormPost, which will behave exactly like generateFormPost in the case of a GET request. Using the same handler for both request methods cuts down on some boilerplate.

widgetFooter :: Widget
widgetFooter = $(whamletFile "templates/footer.hamlet")

widgetMenu :: Widget
widgetMenu = do
    sess <- lookupSession "_USR"
    mUserId <- lookupSession "_ID"    
    $(whamletFile "templates/menu.hamlet")

getAlterarArtigoR :: ArtigoId -> Handler Html
getAlterarArtigoR aid = do
    artigo <- runDB $ get404 aid
    [Entity passoid1 p1, Entity passoid2 p2, Entity passoid3 p3] <- runDB $ selectList [PassosArtigoid ==. aid] [Asc PassosId]  
    categoria  <- runDB $ selectFirst [CategoriaId ==. artigoCategoriaid artigo] []
    [Entity iid info] <- runDB $ selectList [InfoaddArtigoid ==. aid][]
    ((res, widget), enctype) <- runFormPost $ formEditar
        (artigoNome artigo) (artigoCategoriaid artigo) (artigoDataInc artigo) (artigoQtVisualizacao artigo) (artigoQtCurtidas artigo) (artigoQtNaoCurtidas artigo) (artigoQtAcesso artigo)
        (passoid1) (passosTitulo p1) (passosDesc p1)
        (passoid2) (passosTitulo p2) (passosDesc p2)
        (passoid3) (passosTitulo p3) (passosDesc p3)
        (iid) (infoaddObservacoes info) (infoaddAviso info) (infoaddMateriaisNec info) (aid)
    case res of
        FormSuccess (art, (pid1, p1', f1), (pid2, p2', f2), (pid3, p3', f3), (iid', info')) -> do
            runDB $ do
                replace aid art
                replace iid' info'
                replace pid1 p1'
                replace pid2 p2'
                replace pid3 p3'
                
            liftIO $ do
                when (isJust f1) $ fileMove (fromJust f1) ("static" </> "fotos" </> (show $ fromSqlKey pid1))
                when (isJust f2) $ fileMove (fromJust f2) ("static" </> "fotos" </> (show $ fromSqlKey pid2))
                when (isJust f3) $ fileMove (fromJust f3) ("static" </> "fotos" </> (show $ fromSqlKey pid3))
            redirect HomeR
        _ -> defaultLayout $ do 
            setTitle "Artigo"
            addStylesheet $ (StaticR css_bootstrap_css)
            $(whamletFile "templates/alterarartigo.hamlet")
            toWidget $(luciusFile "templates/menu.lucius")
            toWidget $(luciusFile "templates/footer.lucius")
            toWidget $(luciusFile "templates/alterarartigo.lucius")  
            addStylesheet $ (StaticR css_bootstrap_css)

    
postEditarR :: ArtigoId -> Handler Html
postEditarR aid = getAlterarArtigoR aid
        
formEditar :: Text -> CategoriaId -> Day -> Int -> Int -> Int -> Int -> PassosId -> Text -> Textarea -> PassosId -> Text -> Textarea -> PassosId -> Text -> Textarea -> InfoaddId -> Textarea -> Textarea -> Textarea -> ArtigoId
    -> Form (Artigo, (PassosId, Passos, Maybe FileInfo), (PassosId, Passos, Maybe FileInfo), (PassosId, Passos, Maybe FileInfo),(InfoaddId, Infoadd))
formEditar a b c d e f g h i j k l m n o p q r s t u = renderDivs $ (,,,,) 
    <$> (Artigo
    <$> areq textField "Nome do Artigo: " (Just a)
    <*> areq (selectField $ optionsPersistKey [] [] categoriaNome) "Categoria" (Just b)    
    <*> pure c
    <*> pure d
    <*> pure e 
    <*> pure f
    <*> pure g)
    <*> ((,,)
    <$> pure h
    <*> (Passos
    <$> pure u
    <*> areq textField "Passo 1:" (Just i)
    <*> areq textareaField "Descrição:" (Just j))
    <*> aopt fileField FieldSettings{fsId=Just "hident1",
                                         fsLabel="Foto 1: ",
                                         fsTooltip= Nothing,
                                         fsName= Nothing,
                                         fsAttrs=[("accept","image/jpeg")]} 
                           Nothing)
    <*> ((,,)
    <$> pure k
    <*> (Passos
    <$> pure u 
    <*> areq textField "Passo 2:" (Just l)
    <*> areq textareaField "Descrição:" (Just m))
    <*> aopt fileField  FieldSettings{fsId=Just "hident2",
                                         fsLabel="Foto 2: ",
                                         fsTooltip= Nothing,
                                         fsName= Nothing,
                                         fsAttrs=[("accept","image/jpeg")]} 
                           Nothing)
    <*> ((,,)
    <$> pure n
    <*> (Passos
    <$> pure u
    <*> areq textField "Passo 3:" (Just o)
    <*> areq textareaField "Descrição:" (Just p))
    <*> aopt fileField FieldSettings{fsId=Just "hident3",
                                         fsLabel="Foto 3: ",
                                         fsTooltip= Nothing,
                                         fsName= Nothing,
                                         fsAttrs=[("accept","image/jpeg")]} 
                           Nothing)
    <*> ((,)
    <$> pure q
    <*>(Infoadd
    <$> pure u
    <*> areq textareaField "Observações: " (Just r)
    <*> areq textareaField "Avisos: " (Just s)
    <*> areq textareaField "Materiais Necessarios: " (Just t)))	     
    
    
    

getMostrarArtigoR :: ArtigoId -> Handler Html
getMostrarArtigoR aid = do
    artigo <- runDB $ get404 aid
    _ <- runDB $ update aid [ArtigoQtVisualizacao +=. 1]
    [Entity passoid1 p1, Entity passoid2 p2, Entity passoid3 p3] <- runDB $ selectList [PassosArtigoid ==. aid] [Asc PassosId]  
    categoria  <- runDB $ selectFirst [CategoriaId ==. artigoCategoriaid artigo] []
    [Entity iid info] <- runDB $ selectList [InfoaddArtigoid ==. aid][]
    defaultLayout $ do 
        setTitle "Artigo"
        addStylesheet $ (StaticR css_bootstrap_css)
        $(whamletFile "templates/exibirartigo.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")
        toWidget $(luciusFile "templates/alterarartigo.lucius")
        toWidget $(luciusFile "templates/exibirartigo.lucius")
        addStylesheet $ (StaticR css_bootstrap_css)

getCurtirR :: ArtigoId -> Handler Html
getCurtirR aid = do
    _ <- runDB $ update aid [ArtigoQtCurtidas +=. 1]
    artigo <- runDB $ get404 aid
    [Entity passoid1 p1, Entity passoid2 p2, Entity passoid3 p3] <- runDB $ selectList [PassosArtigoid ==. aid] [Asc PassosId]  
    categoria  <- runDB $ selectFirst [CategoriaId ==. artigoCategoriaid artigo] []
    [Entity iid info] <- runDB $ selectList [InfoaddArtigoid ==. aid][]
    defaultLayout $ do 
        setTitle "Artigo"
        addStylesheet $ (StaticR css_bootstrap_css)
        $(whamletFile "templates/exibirartigo.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")
        toWidget $(luciusFile "templates/alterarartigo.lucius")
        toWidget $(luciusFile "templates/exibirartigo.lucius")
        setMessage $ [shamlet| Obrigado pela Curtida!!!|] 
        
        
{-
getNaoCurtirR :: ArtigoId -> Handler Html
getNaoCurtirR aid = do
    _ <- runDB $ update aid [ArtigoQtNaoCurtidas +=. 1]
    artigo <- runDB $ get404 aid
    [Entity passoid1 p1, Entity passoid2 p2, Entity passoid3 p3] <- runDB $ selectList [PassosArtigoid ==. aid] [Asc PassosId]  
    categoria  <- runDB $ selectFirst [CategoriaId ==. artigoCategoriaid artigo] []
    [Entity iid info] <- runDB $ selectList [InfoaddArtigoid ==. aid][]
    defaultLayout $ do 
        setTitle "Artigo"
        addStylesheet $ (StaticR css_bootstrap_css)
        $(whamletFile "templates/exibirartigo.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")
        toWidget $(luciusFile "templates/alterarartigo.lucius")
        toWidget $(luciusFile "templates/exibirartigo.lucius")       
        setMessage $ [shamlet| Obrigado pela sua Opnião!!!|]         
        -}