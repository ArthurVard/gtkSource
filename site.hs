--------------------------------------------------------------------------------
-- gtk.am
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Hakyll
import           Control.Applicative
import           Data.Monoid (mappend, mconcat)
import           Data.Maybe (fromMaybe)
import           Data.Time.Format (formatTime)
import           Data.Time.Clock (getCurrentTime)
import           System.Locale (defaultTimeLocale)
import           System.FilePath.Posix  (takeBaseName, splitDirectories, (</>)
                                                     , addExtension, addExtension
                                                     , replaceExtension)
import           Data.Char (toLower)

--------------------------------------------------------------------------------
main :: IO ()
main = do
   -- get the current year from the system time before entering the Rules monad
   year <- getCurrentYear
   hakyll $ do

    -- compile templates
    match "templates/*" $ compile templateCompiler

    -- copy static assets
    let assets = ["images/*", "CNAME"]

    match (foldr1 (.||.) assets) $ do
        route   idRoute
        compile copyFileCompiler
   
    match ("css/*" .||. "css/icons/*" .||. "foundation/*") $ do
        route   idRoute
        compile compressCssCompiler

    -- build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    
    pageTags <- buildTags ("pages/unifiedexam/*/*.markdown" .||. "pages/unifiedexam/*.markdown")
                          (fromCapture "tags/*.html" . convertToLat)


    newsTags <- buildTags ("pages/news/*/*.markdown" .||."pages/news/*.markdown" )  (fromCapture "tags/news/*.html" . convertToLat)
     
    shtemaranTags <- buildTags ("pages/shtemaran/*/*.markdown" .||."pages/shtemaran/*.markdown" )  (fromCapture "tags/*.html" . convertToLat)

             

    allTags <- buildTags ("posts/*" .||. "pages/unifiedexam/*/*.markdown" .||. "pages/unifiedexam/*.markdown" .||.  "pages/shtemaran/*/*.markdown" .||. "pages/news/*/*.markdown")
                          (fromCapture "tags/*.html" . convertToLat)
     -- base.html needs a year, tag cloud, and the defaults (title/body)
    let baseCtx   = makeDefaultCtx year tags
    let applyBase = loadAndApplyTemplate "templates/base.html" baseCtx

    -- create a specialized post context to handle individual post tags
    let postCtx    = defaultPostCtx tags

    let pageCtx    = defaultPostCtx  pageTags

    let shtemaranCtx    = defaultPostCtx shtemaranTags

    let allTagCtx = defaultPostCtx allTags

    let newsCtx = defaultPostCtx newsTags
        
    
    -- our only root level static page
    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= applyBase
            >>= relativizeUrls

   
    let processPagesRoute root path = root </>  year </> (replaceExtension fileName "html")
                                        where fileName =  last (splitDirectories path)
                                              year | length (splitDirectories path) < 4  = ""
                                                   | otherwise = last $ init (splitDirectories path)
                                                    

    -- render unifiedexams menu pages
    match ("pages/unifiedexam/*/*.markdown"  .||. "pages/unifiedexam/*.markdown" .||. "pages/unifiedexam/*/*.html") $ do
        route $ customRoute $  (processPagesRoute "exam") .  toFilePath 
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/page.html"   (makePageCtx pageTags)
            >>= loadAndApplyTemplate "templates/unifiedexams.html"   (makeUnifiedCtx pageTags)
            >>= loadAndApplyTemplate "templates/base.html" (makeBasePageCtx year) --postCtx1
            >>= relativizeUrls
            
    -- render currentexams menu pages
    match "pages/currentexam/*.markdown" $ do
        route $ customRoute $ (processPagesRoute "exam") .  toFilePath 
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/page.html"   (makePageCtx pageTags)
            >>= loadAndApplyTemplate "templates/index.html"  (makeUnifiedCtx pageTags)
            >>= loadAndApplyTemplate "templates/base.html"   (makeBasePageCtx year)
            >>= relativizeUrls

    -- render contest  menu pages
    match "pages/contest/*.markdown" $ do
        route $ customRoute $ (processPagesRoute "contest") .  toFilePath 
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/page.html"   (makePageCtx pageTags)
            >>= loadAndApplyTemplate "templates/index.html"  (makeUnifiedCtx pageTags)
            >>= loadAndApplyTemplate "templates/base.html"   (makeBasePageCtx year)
            >>= relativizeUrls

    -- render shtemaran menu pages
    match ("pages/shtemaran/*/*.markdown" .||. "pages/shtemaran/*.markdown")$ do
        route $ customRoute $ (processPagesRoute "shtemaran") .  toFilePath 
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/page.html"   (makePageCtx shtemaranTags)
            >>= loadAndApplyTemplate "templates/shtemaran.html"   (makeUnifiedCtx shtemaranTags)
            >>= loadAndApplyTemplate "templates/base.html" (makeBasePageCtx year)
            >>= relativizeUrls   


   -- render news
    match "pages/news/*/*.markdown" $ do
        route $ customRoute $ (processPagesRoute "news") .  toFilePath 
        compile $ pandocCompiler
            >>= saveSnapshot "content"    
            >>= loadAndApplyTemplate "templates/page.html"   (makePageCtx newsTags)
            >>= loadAndApplyTemplate "templates/index.html"   (makeUnifiedCtx newsTags)
            >>= loadAndApplyTemplate "templates/base.html" (makeBasePageCtx year)
            >>= relativizeUrls
   
    -- render each of the individual posts
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "content"    
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" baseCtx --postCtx1
            >>= relativizeUrls

    -- post listings by tag
    tagsRules tags $ \tag pattern -> do
        let title = "Posts պիտակված՝  &#8216;" ++ tag ++ "&#8217;"
        route idRoute
        compile $ do
             
            posts <- constField "posts" <$> postList pattern postCtx recentFirst
            let ctxx = mconcat  [constField "title" title , baseCtx ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"  posts
                >>= loadAndApplyTemplate "templates/default.html"  ctxx
                >>= relativizeUrls
                
    -- pages listings by tag
    tagsRules pageTags $ \tag pattern -> do
        let title = "Էջեր պիտակված՝  &#8216;" ++ tag ++ "&#8217;"
        route idRoute
        compile $ do
             
            posts <- constField "posts" <$> postList pattern pageCtx recentFirst
            let ctxx = mconcat  [constField "title" title , (makeDefaultCtx year pageTags) ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"  posts
                >>= loadAndApplyTemplate "templates/unifiedexams.html"  ctxx
                >>= loadAndApplyTemplate "templates/base.html"  ctxx
                >>= relativizeUrls

    -- news tags
    tagsRules newsTags $ \tag pattern -> do
        let title = "Էջեր պիտակված՝ &#8216;" ++ tag ++ "&#8217;"
        route idRoute
        compile $ do
             
            posts <- constField "posts" <$> postList pattern newsCtx recentFirst
            let ctxx = mconcat  [constField "title" title , (makeDefaultCtx year newsTags) ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"  posts
                >>= loadAndApplyTemplate "templates/index.html"  ctxx
                >>= loadAndApplyTemplate "templates/base.html"  ctxx
                >>= relativizeUrls


    -- shtemaran tags
    tagsRules shtemaranTags $ \tag pattern -> do
        let title = "Էջեր պիտակված՝ &#8216;" ++ tag ++ "&#8217;"
        route idRoute
        compile $ do
             
            posts <- constField "posts" <$> postList pattern shtemaranCtx recentFirst
            let ctxx = mconcat  [constField "title" title , (makeDefaultCtx year shtemaranTags) ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"  posts
                >>= loadAndApplyTemplate "templates/shtemaran.html"  ctxx
                >>= loadAndApplyTemplate "templates/base.html"  ctxx
                >>= relativizeUrls

  
   -- all tags rendeirng
    tagsRules allTags $ \tag pattern -> do
        let title = "From All tagged &#8216;" ++ tag ++ "&#8217;"
        route idRoute
        compile $ do
             
            posts <- constField "posts" <$> postList pattern allTagCtx recentFirst
            let ctxx = mconcat  [constField "title" title , (makeDefaultCtx year allTags) ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"  posts
                >>= loadAndApplyTemplate "templates/base.html"  ctxx
                >>= relativizeUrls    


    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "pages/news/*/*.markdown"
            let archiveCtx =
                    listField "posts" baseCtx (return posts) `mappend`
                    constField "title" "News"            `mappend`
                    tagCloudCtx newsTags                     `mappend` 
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" (makeDefaultCtx year newsTags) -- baseCtx
                >>= relativizeUrls



    create ["allTags.html"] $ do
        route idRoute
        compile $ do
            let xCtx =
                    constField "title" "AllTags"            `mappend`
                    tagCloudCtx allTags                     `mappend`
                    tagCloudCtx' "newstagcloud" newsTags     `mappend` 
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/allTagsPage.html" xCtx
                >>= loadAndApplyTemplate "templates/base.html" baseCtx 
                >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "pages/news/*/*.markdown"
            let indexCtx =
                    listField "posts" newsCtx (return posts) `mappend`
                    constField "title" "Նորություններ"         `mappend`
                    tagCloudCtx newsTags                     `mappend`
                    defaultPostCtx newsTags

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= applyBase
                >>= relativizeUrls
      
    


--------------------------------------------------------------------------------

-- | Creates a "year" context from a string representation of the current year
yearCtx :: String -> Context String
yearCtx year = field "year" $ \item -> return year

-- | Given a collection of Tags, builds a context with a rendered tag cloud
tagCloudCtx :: Tags -> Context String
tagCloudCtx tags = field "tagcloud" $ \item -> rendered
  where rendered = renderTagCloud 85.0 165.0 tags

-- | Given a collection of Tags, builds a context with a rendered tag cloud
tagCloudCtx' :: String -> Tags -> Context String
tagCloudCtx' tagField tags = field tagField $ \item -> rendered
  where rendered = renderTagCloud 85.0 165.0 tags
        

-- | Creates the default/base context used on all pages
makeDefaultCtx :: String -> Tags -> Context String
makeDefaultCtx year tags = mconcat
  [ defaultContext
  , yearCtx     year
  , constField "title"  "ggg"
  , tagCloudCtx tags
  ]

-- | Creates the base page context
makeBasePageCtx :: String -> Context String
makeBasePageCtx year = mconcat
  [ defaultContext
  , yearCtx     year
  ]  

-- | Creates the menu pages context used on all menu pages
makePageCtx :: Tags -> Context String
makePageCtx tags = mconcat
  [ 
    tagsField "tags" tags
  , defaultContext
  ]  

-- | Creates the menu pages context used on all menu pages
makeUnifiedCtx :: Tags -> Context String
makeUnifiedCtx tags = mconcat
  [ 
    tagCloudCtx tags
  , defaultContext
  ]  

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "title" "tt" `mappend`
    defaultContext

-- | Creates the default post context used by pages with posts/post listings
defaultPostCtx :: Tags -> Context String
defaultPostCtx tags = mconcat
  [ dateField "date" "%B %e, %Y"
  , tagsField "tags" tags
  , defaultContext
  ]

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


-- -----------------------------------------------------------------------------
-- * Helpers

-- | Because I never remember to update the copyright in the footer
getCurrentYear :: IO String
getCurrentYear = formatTime defaultTimeLocale "%Y" <$> getCurrentTime

-- | Builds a pattern to match only posts tagged with a given primary tag.
-- For instance, only matching posts tagged "code" on the explore/code page.
explorePattern :: Tags -> String -> Pattern
explorePattern tags primaryTag = fromList identifiers
  where identifiers = fromMaybe [] $ lookup primaryTag (tagsMap tags)


-- -----------------------------------------------------------------------------
-- * Compilers

-- | Creates a compiler to render a list of posts for a given pattern, context,
-- and sorting/filtering function
postList :: Pattern
         -> Context String
         -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList pattern postCtx sortFilter = do
    posts   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody "templates/post-item.html"
    applyTemplateList itemTpl postCtx posts



armToLat 'ա' = "a"
armToLat 'բ' = "b"
armToLat 'գ' = "g"
armToLat 'դ' = "d"
armToLat 'ե' = "e"
armToLat 'զ' = "z"
armToLat 'է' = "e"
armToLat 'ը' = "y"
armToLat 'թ' = "t"
armToLat 'ժ' = "j"
armToLat 'ի' = "i"
armToLat 'լ' = "l"
armToLat 'խ' = "kh"
armToLat 'ծ' = "ts"
armToLat 'կ' = "k"
armToLat 'հ' = "h"
armToLat 'ձ' = "dz"
armToLat 'ղ' = "gh"
armToLat 'ճ' = "j"
armToLat 'մ' = "m"
armToLat 'յ' = "y"
armToLat 'ն' = "n"
armToLat 'շ' = "sh"
armToLat 'ո' = "o"
armToLat 'չ' = "ch"
armToLat 'պ' = "p"
armToLat 'ջ' = "j"
armToLat 'ռ' = "r"
armToLat 'ս' = "s"
armToLat 'վ' = "v"
armToLat 'տ' = "t"
armToLat 'ր' = "r"
armToLat 'ց' = "c"
armToLat 'ւ' = "u"
armToLat 'փ' = "ph"
armToLat 'ք' = "q"
armToLat 'և' = "ev"
armToLat 'օ' = "o"
armToLat 'ֆ' = "f"
armToLat x = [x]

convertToLat :: String -> String
convertToLat "նորություն" = "news"
convertToLat "նորություններ" = "news"
convertToLat str  = Prelude.concat  $ map (armToLat . toLower) str



