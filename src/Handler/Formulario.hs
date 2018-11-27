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
                       ,c :: Day        -- data da inclusao
                       ,w :: Int
                       ,x :: Int
                       ,y :: Int
                       ,z :: Int
                       ,d :: Text           -- titulo do passo
                       ,f :: Textarea       -- descricao do passo
                       ,g :: Textarea       -- dicas
                       ,h :: Textarea       -- aviso
                       ,i :: Textarea       -- materiaisNec         
                    }

formeArt :: Forme -> Artigo
formeArt(Forme a b c w x y z d f g h i) = (Artigo b a c w x y z)
                    
formePasso :: Forme -> ArtigoId -> Passos
formePasso (Forme a b c w x y z d f g h i) m = (Passos m d f)
                    
formeInfo :: Forme -> ArtigoId -> Infoadd
formeInfo (Forme a b c w x y z d f g h i)m = (Infoadd m g h i)
   
                                        
formeArtigo :: Day -> Form Forme
formeArtigo x2 = renderBootstrap $ (Forme 
        <$> areq (selectField listaCategoria) FieldSettings{fsId=Just "li",
                           fsLabel="Categoria :",
                           fsTooltip= Nothing,
                           fsName= Nothing,
                           fsAttrs=[("class","form-control"),("placeholder","EX: Policarpo Quaresma"),("accept","image/jpeg")]} Nothing
                           
        <*> areq textField "Nome da Dica: " Nothing
        <*> pure x2 
        <*> pure 0
        <*> pure 0 
        <*> pure 0
        <*> pure 0
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
       
