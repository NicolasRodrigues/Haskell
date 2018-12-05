{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Cadastro where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql
import Handler.Formulario

formCadastro :: Day -> Form (Artigo, (Passos, FileInfo), (Passos, FileInfo), (Passos, FileInfo),Infoadd)
formCadastro x2 = renderDivs $ (,,,,) 
    <$> (Artigo
    <$> areq textField "Nome do Artigo: " Nothing
    <*> areq (selectField $ optionsPersistKey [] [] categoriaNome) "Categoria" Nothing    
    <*> pure x2
    <*> pure 0
    <*> pure 0 
    <*> pure 0
    <*> pure 0)
    <*> ((,)
    <$> (Passos
    <$> pure (toSqlKey 0)
    <*> areq textField "Passo 1:" Nothing
    <*> areq textareaField "Descrição:" Nothing)
    <*> areq fileField FieldSettings{fsId=Just "hident1",
                                         fsLabel="Foto 1: ",
                                         fsTooltip= Nothing,
                                         fsName= Nothing,
                                         fsAttrs=[("accept","image/jpeg")]} 
                           Nothing)
    <*> ((,)
    <$> (Passos
    <$> pure (toSqlKey 0)
    <*> areq textField "Passo 2:" Nothing
    <*> areq textareaField "Descrição:" Nothing)
    <*> areq fileField  FieldSettings{fsId=Just "hident1",
                                         fsLabel="Foto 2: ",
                                         fsTooltip= Nothing,
                                         fsName= Nothing,
                                         fsAttrs=[("accept","image/jpeg")]} 
                           Nothing)
    <*> ((,)
    <$> (Passos
    <$> pure (toSqlKey 0)
    <*> areq textField "Passo 3:" Nothing
    <*> areq textareaField "Descrição:" Nothing)
    <*> areq fileField 
                           FieldSettings{fsId=Just "hident1",
                                         fsLabel="Foto 3: ",
                                         fsTooltip= Nothing,
                                         fsName= Nothing,
                                         fsAttrs=[("accept","image/jpeg")]} 
                           Nothing)
    <*> (Infoadd
    <$> pure (toSqlKey 0)
    <*> areq textareaField "Observações: " Nothing
    <*> areq textareaField "Avisos: " Nothing
    <*> areq textareaField "Materiais Necessarios: " Nothing)	                    

   
widgetFooter :: Widget
widgetFooter = $(whamletFile "templates/footer.hamlet")

widgetMenu :: Widget
widgetMenu = $(whamletFile "templates/menu.hamlet")


diaHj :: IO Day
diaHj = fmap utctDay getCurrentTime 

getCadastroR :: Handler Html
getCadastroR = do
    diaInc <- liftIO diaHj
    (wid, enc) <- generateFormPost (formCadastro diaInc)
    defaultLayout $ do
        $(whamletFile "templates/cadastro.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")
        toWidget $(luciusFile "templates/cadastro.lucius")

postCadastroR :: Handler Html
postCadastroR = do
    diaInc <- liftIO diaHj
    ((res, _), _) <- runFormPost (formCadastro diaInc)
    case res of
        FormSuccess (art, (p1, f1), (p2, f2), (p3, f3),info) -> do
            (pid1, pid2, pid3) <- runDB $ do
                aid <- insert art
                inf1 <- insert $ info {infoaddArtigoid =aid}                
                pid1 <- insert $ p1 {passosArtigoid = aid} 
                pid2 <- insert $ p2 {passosArtigoid = aid}
                pid3 <- insert $ p3 {passosArtigoid = aid}
                return (pid1, pid2, pid3)
            liftIO $ fileMove f1 ("static" </> "fotos" </> (show $ fromSqlKey pid1))
            liftIO $ fileMove f2 ("static" </> "fotos" </> (show $ fromSqlKey pid2))
            liftIO $ fileMove f3 ("static" </> "fotos" </> (show $ fromSqlKey pid3))
            redirect ArtigoR
        _ -> redirect CadastroR 
        

               
{-

getCadastro1R :: ArtigoId -> Handler Html
getCadastro1R aid = do
    diaInc <- liftIO diaHj
    artigo <- runDB $ get404 aid 
    [passo1,passo2,passo3]  <- runDB selectList [PassosArtigoId ==. aid][]
    Just info   <- runDB selectFirst  [InfoaddArtigoId ==. aid][]

        
    (wid, enc) <- generateFormPost (formCadastro1 (artigoNome artigo) (artigoCategoriaid artigo) (diaInc) (artigoQtVisualizacao)
                                    (artigoQtCurtidas artigo) (artigoQtNaoCurtidas artigo) (artigoQtAcesso artigo)
                                    (entityKey passo1) (passosTitulo $ entityVal passo1) (passosDesc $ entityVal passo1)
                                    (entityKey passo2) (passosTitulo $ entityVal passo2) (passosDesc $ entityVal passo2)
                                    (entityKey passo3) (passosTitulo $ entityVal passo3) (passosDesc $ entityVal passo3)
                                    (entityKey info)   (infoaddObservacoes $ entityVal info) 
                                    (infoaddAviso $ entityVal info) (infoaddMateriaisNec $ entityVal info) (aid))  
    defaultLayout $ do
        $(whamletFile "templates/cadastro.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")
        toWidget $(luciusFile "templates/cadastro.lucius")        

postCadastro1R :: ArtigoId -> Handler Html
postCadastro1R aid = do
    diaInc <- liftIO diaHj
    artigo <- runDB $ get404 aid 
    [passo1,passo2,passo3]  <- runDB selectList [PassosArtigoId ==. aid][]
    Just info   <- runDB selectFirst  [InfoaddArtigoId ==. aid][]

    ((res,_),_) <- runFormPost (formCadastro1 (artigoNome artigo) (artigoCategoriaid artigo) (diaInc) (artigoQtVisualizacao)
                                    (artigoQtCurtidas artigo) (artigoQtNaoCurtidas artigo) (artigoQtAcesso artigo)
                                    (entityKey passo1) (passosTitulo $ entityVal passo1) (passosDesc $ entityVal passo1)
                                    (entityKey passo2) (passosTitulo $ entityVal passo2) (passosDesc $ entityVal passo2)
                                    (entityKey passo3) (passosTitulo $ entityVal passo3) (passosDesc $ entityVal passo3)
                                    (entityKey info)   (infoaddObservacoes $ entityVal info) 
                                    (infoaddAviso $ entityVal info) (infoaddMateriaisNec $ entityVal info))    

    case res of
        FormSuccess(art, (p1, f1), (p2, f2), (p3, f3),info) -> do 
                runDB $ replace aid art 
                setMessage [shamlet|
                    <h1>
                        Categoria cadastrado!
                |]
                redirect HomeR        

transf:: Artigo -> Maybe Artigo
transf x = Nothing 
transf x = Just x



-}