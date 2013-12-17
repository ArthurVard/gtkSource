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

     -- base.html needs a year, tag cloud, and the defaults (title/body)
    let baseCtx   = makeDefaultCtx year tags
    let applyBase = loadAndApplyTemplate "templates/default.html" baseCtx

    -- create a specialized post context to handle individual post tags
    let postCtx    = defaultPostCtx tags

    
    -- our only root level static page
    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= applyBase
            >>= relativizeUrls

   -- render menu pages
    match "pages/unifiedexams/*" $ do
        route idRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/unifiedexams.html"    defaultContext
            >>= loadAndApplyTemplate "templates/default.html" baseCtx --postCtx1
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
        let title = "Posts tagged &#8216;" ++ tag ++ "&#8217;"
        route idRoute
        compile $ do
             
            posts <- constField "posts" <$> postList pattern postCtx recentFirst
            let ctxx = mconcat  [constField "title" title , baseCtx ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"  posts
                >>= loadAndApplyTemplate "templates/default.html"  ctxx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" baseCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" baseCtx --(indexarchiveCtx "posts" posts)
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultPostCtx tags

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= applyBase --loadAndApplyTemplate "templates/default.html" (mainindexCtx "posts" posts)
                >>= relativizeUrls
      
    


--------------------------------------------------------------------------------

-- | Creates a "year" context from a string representation of the current year
yearCtx :: String -> Context String
yearCtx year = field "year" $ \item -> return year

-- | Given a collection of Tags, builds a context with a rendered tag cloud
tagCloudCtx :: Tags -> Context String
tagCloudCtx tags = field "tagcloud" $ \item -> rendered
  where rendered = renderTagCloud 85.0 165.0 tags

-- | Creates the default/base context used on all pages
makeDefaultCtx :: String -> Tags -> Context String
makeDefaultCtx year tags = mconcat
  [ defaultContext
  , yearCtx     year
  , constField "title"  "ggg"
  , tagCloudCtx tags
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
