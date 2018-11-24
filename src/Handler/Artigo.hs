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
    passos <- runDB $ selectList [PassosArtigoid ==. aid] [Asc PassosId]
    artigo <- runDB $ get404 aid
    defaultLayout 
        [whamlet|
        <table class=table>
            <thead>
                <tr>
                    <th>
                        Nome:
                    <td>
                        #{artigoNome artigo}
                    <th>
                        Categoria:
                    <td>
                       
                    <th>
                        Data do artigo
                    <td>
                <tr>        
                    <th>
                        Passo 1: 
                    <td>
                        
                <tr>
                    <th>
                        Descrição:
                    <td>
                        
                <tr>
                    <th>
                        Passo 2:
                    <td>
                    
                <tr>
                    <th>
                        Descrição:
                    <td>
                    
                <tr>
                    <th>
                        Passos 3:
                <tr>
                        Teste
                    <td>
                        #{artigoNome artigo}  
                    <td>
                        <label>
                            
                        #{show $ artigoDataArt artigo}
               
                    $forall (Entity _ passo) <- passos
                        <td>
                            #{passosTitulo passo}
        |]
