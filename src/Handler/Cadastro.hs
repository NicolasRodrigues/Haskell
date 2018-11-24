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

formCadastro :: Form (Artigo, (Passos, FileInfo), (Passos, FileInfo), (Passos, FileInfo))
formCadastro = renderDivs $ (,,,) <$> (Artigo
    <$> areq (selectField $ optionsPersistKey [] [] categoriaNome) "Categoria" Nothing
    <*> areq textField "Nome da dica: " Nothing
    <*> lift (liftIO getCurrentTime))
    <*> ((,)
    <$> (Passos
    <$> pure (toSqlKey 0)
    <*> areq textField "Titulo1" Nothing
    <*> areq textareaField "Descrição1" Nothing)
    <*> areq fileField FieldSettings{fsId=Just "hident1",
                                         fsLabel="Foto 1: ",
                                         fsTooltip= Nothing,
                                         fsName= Nothing,
                                         fsAttrs=[("accept","image/jpeg")]} 
                           Nothing)
    <*> ((,)
    <$> (Passos
    <$> pure (toSqlKey 0)
    <*> areq textField "Titulo2" Nothing
    <*> areq textareaField "Descrição2" Nothing)
    <*> areq fileField  FieldSettings{fsId=Just "hident1",
                                         fsLabel="Foto 2: ",
                                         fsTooltip= Nothing,
                                         fsName= Nothing,
                                         fsAttrs=[("accept","image/jpeg")]} 
                           Nothing)
    <*> ((,)
    <$> (Passos
    <$> pure (toSqlKey 0)
    <*> areq textField "Titulo3" Nothing
    <*> areq textareaField "Descrição3" Nothing)
    <*> areq fileField 
                           FieldSettings{fsId=Just "hident1",
                                         fsLabel="Foto 3: ",
                                         fsTooltip= Nothing,
                                         fsName= Nothing,
                                         fsAttrs=[("accept","image/jpeg")]} 
                           Nothing)
    
    
   
widgetFooter :: Widget
widgetFooter = $(whamletFile "templates/footer.hamlet")

widgetMenu :: Widget
widgetMenu = $(whamletFile "templates/menu.hamlet")


getCadastroR :: Handler Html
getCadastroR = do
    (wid, enc) <- generateFormPost formCadastro
    defaultLayout $ do
        $(whamletFile "templates/cadastro.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")
        toWidget $(luciusFile "templates/cadastro.lucius")

postCadastroR :: Handler Html
postCadastroR = do
    ((res, _), _) <- runFormPost formCadastro
    case res of
        FormSuccess (art, (p1, f1), (p2, f2), (p3, f3)) -> do
            (pid1, pid2, pid3) <- runDB $ do
                aid <- insert art
                pid1 <- insert $ p1 {passosArtigoid = aid} 
                pid2 <- insert $ p2 {passosArtigoid = aid}
                pid3 <- insert $ p3 {passosArtigoid = aid}
                return (pid1, pid2, pid3)
            liftIO $ fileMove f1 ("static" </> "fotos" </> (show $ fromSqlKey pid1))
            liftIO $ fileMove f2 ("static" </> "fotos" </> (show $ fromSqlKey pid2))
            liftIO $ fileMove f3 ("static" </> "fotos" </> (show $ fromSqlKey pid3))
            redirect ArtigoR
        _ -> redirect CadastroR 