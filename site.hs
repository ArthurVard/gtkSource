--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do


  -- copy static assets
    let assets = ["images/*", "CNAME"]

    match (foldr1 (.||.) assets) $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx1
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" (indexarchiveCtx "posts" posts)
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" (mainindexCtx "posts" posts)
                >>= relativizeUrls
      
    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

postCtx1 =
      constField "mainselection" " "    `mappend`
      constField "aboutselection" " "   `mappend`
      constField "contactselection" " " `mappend`
      constField "archiveselection" " " `mappend`
      defaultContext


      --context for archive, or any generated file to process with default.hetml temaplate
indexarchiveCtx postToken ps =
        listField postToken  postCtx (return ps)         `mappend`
        constField "title" "Archive"                     `mappend`
        constField "mainselection" " "                   `mappend`
        constField "aboutselection" " "                  `mappend`
        constField "contactselection" " "                `mappend`
        constField "archiveselection" "class='selected'" `mappend`
        defaultContext

mainindexCtx postToken ps =
          listField postToken  postCtx (return ps)      `mappend`
          constField "title" "Home"                     `mappend`
          constField "mainselection" "class='selected'" `mappend`
          constField "aboutselection" " "               `mappend`
          constField "contactselection" " "             `mappend`
          constField "archiveselection" " "             `mappend`
          defaultContext
