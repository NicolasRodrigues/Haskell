{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies, DeriveGeneric #-}
module Handler.Formulario where

import System.Directory
import Import
import Database.Persist.Postgresql
import Text.Julius


data Forme = Forme {    a :: CategoriaId    -- categoriaid
                       ,b :: Text           -- nome artigo 
                       ,c :: UTCTime        -- data da inclusao
                       ,d :: Text           -- titulo do passo
                       ,f :: Textarea       -- descricao do passo
                       ,g :: Textarea       -- dicas
                       ,h :: Textarea       -- aviso
                       ,i :: Textarea       -- materiaisNec         
                    }

formeArt :: Forme -> Artigo
formeArt(Forme a b c d f g h i) = (Artigo a b c)
                    
formePasso :: Forme -> ArtigoId -> Passos
formePasso (Forme a b c d f g h i) m = (Passos m d f)
                    
formeInfo :: Forme -> ArtigoId -> InfoAdicional
formeInfo(Forme a b c d f g h i) m = (InfoAdicional m g h i)
   
                                        
formeArtigo :: UTCTime -> Form Forme
formeArtigo x2 = renderBootstrap $ (Forme 
        <$> areq (selectField listaCategoria) FieldSettings{fsId=Just "li",
                           fsLabel="Categoria :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Policarpo Quaresma")]} Nothing
                           
        <*> areq textField "Nome da Dica: " Nothing
        <*> pure x2 
        <*> areq textField "Título: " Nothing
        <*> areq textareaField FieldSettings{fsId=Just "campo4",
                           fsLabel="Descricao :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Em uma terra muito muito distante..."),("style","display:inline-block")]} Nothing
        <*> areq textareaField FieldSettings{fsId=Just "campo4",
                           fsLabel="Dicas :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Em uma terra muito muito distante..."),("style","display:inline-block")]} Nothing
        <*> areq textareaField FieldSettings{fsId=Just "campo4",
                           fsLabel="Avisos :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Em uma terra muito muito distante..."),("style","display:inline-block")]} Nothing
        <*> areq textareaField FieldSettings{fsId=Just "campo4",
                           fsLabel="Materias Necessarios :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Em uma terra muito muito distante..."),("style","display:inline-block")]} Nothing
 

    )                                        
listaCategoria = do
       entidades <- runDB $ selectList [] [Asc CategoriaNome] 
       optionsPairs $ fmap (\ent -> (categoriaNome $ entityVal ent, entityKey ent)) entidades   