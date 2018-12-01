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



getMostraArtigoR :: ArtigoId -> Handler Html
getMostraArtigoR aid = do
    artigo <- runDB $ get404 aid
    passos <- runDB $ selectList [PassosArtigoid ==. aid] [Asc PassosId]  
    categoria  <- runDB $ selectFirst [CategoriaId ==. artigoCategoriaid artigo] []
    info <- runDB $ selectList [InfoaddArtigoid ==. aid][]

    defaultLayout $ do 
        setTitle "Artigo"
        addStylesheet $ (StaticR css_bootstrap_css)
        [whamlet|
        <table class=table>
            <thead>
                <tr>
                    <th>
                        Nome do Artigo: #{artigoNome artigo}
                <tr>
                    <th>
                       $forall (Entity _ cat)  <- categoria
                        Categoria: #{categoriaNome cat}
                <tr>
                    <th>
                        Data do artigo: #{show $ artigoDataInc artigo}

                    $forall (Entity chave passo) <- passos
                        <tr>
                            <th>
                                <img src="https://projhaskell-danfatec13.c9users.io/static/fotos/#{fromSqlKey $ chave}">                    
                        <tr>
                            <th>
                                Titulo Passo: #{passosTitulo passo}
                        <tr>        
                            <th>
                                Descrição:  #{passosDesc passo}
                <tr>
                    <th>
                        Qt Visualização: #{artigoQtVisualizacao artigo}
                <tr>
                    <th>
                        Qt Curtidas: #{artigoQtCurtidas artigo}
                <tr>
                    <th>
                        Qt Não Curtidas: #{artigoQtNaoCurtidas artigo}
                $forall (Entity _ inf) <- info        
                    <tr>
                        <th>
                            Observações: #{infoaddObservacoes inf}
                    <tr>
                        <th>
                            Aviso: #{infoaddAviso inf}
                    <tr>
                        <th>
                            Materiais Necessarios: #{infoaddMateriaisNec inf}
                    
        |]
