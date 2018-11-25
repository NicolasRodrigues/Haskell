[1mdiff --git a/src/Handler/Cadastro.hs b/src/Handler/Cadastro.hs[m
[1mindex 0745d08..19992a4 100644[m
[1m--- a/src/Handler/Cadastro.hs[m
[1m+++ b/src/Handler/Cadastro.hs[m
[36m@@ -12,7 +12,8 @@[m [mimport Text.Julius[m
 import Database.Persist.Sql[m
 [m
 formCadastro :: Form (Artigo, (Passos, FileInfo), (Passos, FileInfo), (Passos, FileInfo))[m
[31m-formCadastro = renderDivs $ (,,,) <$> (Artigo[m
[32m+[m[32mformCadastro = renderDivs $ (,,,)[m[41m [m
[32m+[m[32m    <$> (Artigo[m
     <$> areq (selectField $ optionsPersistKey [] [] categoriaNome) "Categoria" Nothing[m
     <*> areq textField "Nome da dica: " Nothing[m
     <*> lift (liftIO getCurrentTime))[m
[36m@@ -60,7 +61,7 @@[m [mpostCadastroR = do[m
                 aid <- insert art[m
                 pid1 <- insert $ p1 {passosArtigoid = aid} [m
                 pid2 <- insert $ p2 {passosArtigoid = aid}[m
[31m-                pid3 <- insert $ p3 {passosArtigoid = aid}[m
[32m+[m[32m                pid3 <- insert $ p3 {passosArtigoid = aid}[m[41m             [m
                 return (pid1, pid2, pid3)[m
             liftIO $ fileMove f1 ("static" </> "fotos" </> (show $ fromSqlKey pid1))[m
             liftIO $ fileMove f2 ("static" </> "fotos" </> (show $ fromSqlKey pid2))[m
[1mdiff --git a/src/Handler/Teste.hs b/src/Handler/Teste.hs[m
[1mindex 93c3bdd..de252b9 100644[m
[1m--- a/src/Handler/Teste.hs[m
[1m+++ b/src/Handler/Teste.hs[m
[36m@@ -14,6 +14,7 @@[m [mimport Control.Monad.Zip[m
 import Yesod.Form[m
 import Handler.Formulario[m
 [m
[32m+[m
 formUsuario :: Form (Usuario, Text)[m
 formUsuario = renderBootstrap $  pure (,)[m
     <*> (Usuario [m
[36m@@ -220,9 +221,18 @@[m [mdeleteApagarArtigoR artigoid = do[m
 -- para trazer todos as categorias .[m
 getListArtigosR :: Handler TypedContent [m
 getListArtigosR = do [m
[31m-    artigos <- runDB $ selectList [] [Asc ArtigoNome][m
[31m-    sendStatusJSON ok200 (object ["resp" .= artigos]) [m
[32m+[m[32m      artigos <- runDB $ selectList [] [Asc ArtigoNome][m
[32m+[m[32m      sendStatusJSON ok200 (object ["resp" .= artigos])[m[41m [m
      [m
[32m+[m[32m--getListArtigosR :: Handler TypedContent[m
[32m+[m[32m--getListArtigosR = do[m[41m [m
[32m+[m[32m   -- userlogado <- lookupSession "_ID"[m
[32m+[m[32m  --  artigos <- runDB $ selectList [] [Asc ArtigoNome][m
[32m+[m[32m    --defaultLayout $ do[m[41m [m
[32m+[m[32m      --  setTitle "Lista de Artigos"[m
[32m+[m[32m    --    addStylesheet $ (StaticR css_bootstrap_css)[m
[32m+[m[32m      --  toWidget $ $(whamletFile "templates/listarArtigos.hamlet")[m[41m [m
[32m+[m[41m            [m
     [m
 --formPasso :: Form Passos[m
 ---formPasso = renderBootstrap $ (Passos[m
