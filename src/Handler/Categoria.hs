{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Categoria where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Sql
import Control.Monad.Zip
import Yesod.Form
	
-- deletar a categoria de acordo com o categoriaId recebido
getApagarCategoriaR :: CategoriaId ->  Handler Html
getApagarCategoriaR caid = do  
    runDB $ deleteCascade  caid
    redirect ExibirCategoriaR    


widgetFooter :: Widget
widgetFooter = $(whamletFile "templates/footer.hamlet")

widgetMenu :: Widget
widgetMenu = do
    sess <- lookupSession "_USR"
    mUserId <- lookupSession "_ID"    
    $(whamletFile "templates/menu.hamlet")


getAlterarCategoriaR ::CategoriaId -> Handler Html
getAlterarCategoriaR aid = do 
    [Entity iid info] <- runDB $ selectList [CategoriaId ==. aid][]
    ((res,widget), enctype) <- runFormPost $ formCategoria (categoriaNome info) 
    case res of
        FormSuccess sob -> do
            runDB $ replace iid sob
            redirect HomeR
        _ -> defaultLayout $ do 
            setTitle "Alterar categoria"
            addStylesheet $ (StaticR css_bootstrap_css)        
            $(whamletFile "templates/alterarcategoria.hamlet")
            toWidget $(luciusFile "templates/menu.lucius")
            toWidget $(luciusFile "templates/footer.lucius")
        
formCategoria ::  Text -> Form Categoria
formCategoria a = renderBootstrap $ (Categoria 
        <$> areq textField "Nome:" (Just a)
    )        

postAlterarCategoria2R :: CategoriaId -> Handler Html
postAlterarCategoria2R aid = getAlterarCategoriaR aid
            
            


getExibirCategoriaR :: Handler Html
getExibirCategoriaR = do 
    cat <- runDB $ selectList [][Asc CategoriaId]
    defaultLayout $ do 
        setTitle "Exibir Categoria"
        addStylesheet $ (StaticR css_bootstrap_css)        
        $(whamletFile "templates/exibircategoria.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")        
        
        
formCategoria1 :: Form Categoria
formCategoria1 = renderBootstrap $   (Categoria 
        <$> areq textField "Nome: " Nothing 
    )

getCategoriaR :: Handler Html
getCategoriaR = do 
    (widgetUsu, enctype) <- generateFormPost formCategoria1
    msg <- getMessage
    defaultLayout $ do
        setTitle "Categoria"
        addStylesheet $ (StaticR css_bootstrap_css)        
        $(whamletFile "templates/categoria.hamlet")
        toWidget $(luciusFile "templates/menu.lucius")
        toWidget $(luciusFile "templates/footer.lucius")
        
        
postCategoriaR :: Handler Html
postCategoriaR = do 
    ((res,_),_) <- runFormPost formCategoria1
    case res of
        FormSuccess (cat) -> do 
                runDB $ insert cat 
                setMessage [shamlet|
                    <h1>
                        Categoria cadastrado!
                |]
                redirect HomeR        