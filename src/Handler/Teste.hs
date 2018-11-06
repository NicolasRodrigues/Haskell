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

getListAlunoR :: Handler TypedContent
getListAlunoR = do 
    alunos <- runDB $ selectList [] [Asc AlunoNome]
    sendStatusJSON ok200 (object ["resp" .= alunos])

postInsereAlunoR :: Handler TypedContent
postInsereAlunoR = do
    aluno <- requireJsonBody :: Handler Aluno
    alunoid <- runDB $ insert aluno
    sendStatusJSON created201 (object ["resp" .= alunoid])