--------------------------------------------------------------------------------
-- gtk.am
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Control.Applicative
import           Data.Monoid (mappend, mconcat)
import           Data.List (isInfixOf, intersperse, intercalate, sortBy, reverse)
import           Data.Maybe (fromMaybe)
import           Data.Time.Format (formatTime)
import           Data.Time.Clock (getCurrentTime)
import           System.Locale (defaultTimeLocale)
import           System.FilePath.Posix  (takeBaseName, splitDirectories, (</>)
                                                     , addExtension, addExtension
                                                     , replaceExtension, dropExtension)
import           Data.Char (toLower)
import           Text.Blaze.Html.Renderer.String (renderHtml)
import           Text.Blaze ((!), toValue)
import qualified Data.Map as M
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Hakyll
--------------------------------------------------------------------------------

-- | Number of news teasers displayed per sorted index page.
--
newsPerIndexPage :: Int
newsPerIndexPage = 3


main :: IO ()
main = do
   -- get the current year from the system time before entering the Rules monad
   year <- getCurrentYear
   hakyll $ do

    -- compile templates
    match "templates/*" $ compile templateCompiler

    -- copy static assets
    let assets = ["images/*", "images/books/*",  "CNAME"]

    match (foldr1 (.||.) assets) $ do
        route   idRoute
        compile copyFileCompiler
   
    match ("css/*" .||. "css/icons/*" .||. "foundation/*") $ do
        route   idRoute
        compile compressCssCompiler

    -- build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")
    
    unifiedExamTags <- buildTags ( "pages/unifiedexam/*/*.markdown"
                           .||. "pages/unifiedexam/*.markdown")
                (fromCapture "tags/*.html" . convertToLat)

    contestTags <- buildTags ( "pages/contest/*/*.markdown"
                               .||. "pages/contest/*.markdown")
                (fromCapture "tags/contest/*.html" . convertToLat)

    newsTags <- buildTags ("pages/news/*/*.markdown"
                           .||."pages/news/*.markdown" )
                (fromCapture "tags/news/*.html" . convertToLat)

   
     
    shtemaranTags <- buildTags ("pages/shtemaran/*/*.markdown"
                                .||."pages/shtemaran/*.markdown" )
                     (fromCapture "tags/shtemaran/*.html" . convertToLat)
   

    allTags <- buildTags ("posts/*"
                          .||. "pages/unifiedexam/*/*.markdown" 
                          .||. "pages/unifiedexam/*.markdown"
                          .||.  "pages/shtemaran/*/*.markdown"
                          .||. "pages/news/*/*.markdown")
               (fromCapture "tags/*.html" . convertToLat)
               
     -- base.html needs a year, tag cloud, and the defaults (title/body)
    let baseCtx   = makeDefaultCtx year tags
    let applyBase = loadAndApplyTemplate "templates/base.html" baseCtx

    -- create a specialized post context to handle individual post tags
    let postCtx      = defaultPostCtx tags

    let unifiedExamCtx      = defaultPostCtx  unifiedExamTags

    let contestCtx = defaultPostCtx contestTags

    let shtemaranCtx = defaultPostCtx shtemaranTags

    let allTagCtx    = defaultPostCtx allTags

    let newsCtx      = defaultPostCtx newsTags
        
    
    -- our only root level static page
    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= applyBase
            >>= relativizeUrls

   
    -- render unifiedexams menu pages
    match ("pages/unifiedexam/*/*.markdown"
           .||. "pages/unifiedexam/*.markdown"
           .||. "pages/unifiedexam/*/*.html") $ do
        route $ customRoute $  (processPagesRoute "exam") .  toFilePath 
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/page.html"         (makePageCtx unifiedExamTags)
            >>= loadAndApplyTemplate "templates/unifiedexams.html" (makeUnifiedCtx unifiedExamTags)
            >>= loadAndApplyTemplate "templates/base.html"         (makeBasePageCtx year) --postCtx1
            >>= relativizeUrls
            
    -- render currentexams menu pages
    match "pages/currentexam/*.markdown" $ do
        route $ customRoute $ (processPagesRoute "exam") .  toFilePath 
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/page.html"   (makePageCtx unifiedExamTags)
            >>= loadAndApplyTemplate "templates/index.html"  (makeUnifiedCtx unifiedExamTags)
            >>= loadAndApplyTemplate "templates/base.html"   (makeBasePageCtx year)
            >>= relativizeUrls

    -- render contest  menu pages
    match ("pages/contest/*/*.markdown"
           .||. "pages/copntest/*.markdown"
           .||. "pages/contest/*/*.html") $ do
        route $ customRoute $ (processPagesRoute "contest") .  toFilePath
        tgsA <- contestAdvancedCtx  contestTags
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/page.html"     tgsA
            >>= loadAndApplyTemplate "templates/contest.html"  (makeUnifiedCtx contestTags)
            >>= loadAndApplyTemplate "templates/base.html"     (makeBasePageCtx year)
            >>= relativizeUrls

    -- render shtemaran menu pages
   -- match ("pages/shtemaran/*/*.markdown" .||. "pages/shtemaran/*.markdown")$ do
    match "pages/shtemaran/*/*.markdown" $ do     
        route $ customRoute $ (processPagesRoute "shtemaran") .  toFilePath 
        compile $ pandocCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/book.html"      (makePageCtx shtemaranTags) 
            >>= loadAndApplyTemplate "templates/page.html"      (makePageCtx shtemaranTags)
            >>= loadAndApplyTemplate "templates/shtemaran.html" (makeUnifiedCtx shtemaranTags)
            >>= loadAndApplyTemplate "templates/base.html"      (makeBasePageCtx year)
            >>= relativizeUrls   


    -- render news
    match "pages/news/*/*.markdown" $ do
        route $ customRoute $ (processPagesRoute "news") .  toFilePath 
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html"  (makePageCtx newsTags)
            >>= saveSnapshot "news-content"
            >>= loadAndApplyTemplate "templates/index.html" (makeUnifiedCtx newsTags)
            >>= loadAndApplyTemplate "templates/base.html"  (makeBasePageCtx year)
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
             
            posts <- constField "posts" <$> postList pattern postCtx recentFirst "tagItem"
            let ctxx = mconcat  [constField "title" title , baseCtx ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"   posts
                >>= loadAndApplyTemplate "templates/default.html" ctxx
                >>= relativizeUrls


    -- contest listings by tag
    tagsRules contestTags $ \tag pattern -> do
        let title = "Էջեր պիտակված՝  &#8216;" ++ tag ++ "&#8217;"
        route idRoute
        compile $ do
             
            posts <- constField "posts" <$> postList pattern contestCtx recentFirst "tagItem"
            let ctxx = mconcat  [constField "title" title , (makeDefaultCtx year contestTags) ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"   posts
                >>= loadAndApplyTemplate "templates/contest.html" ctxx
                >>= loadAndApplyTemplate "templates/base.html"    ctxx
                >>= relativizeUrls
    
    -- unifiedExams listings by tag
    tagsRules unifiedExamTags $ \tag pattern -> do
        let title = "Էջեր պիտակված՝  &#8216;" ++ tag ++ "&#8217;"
        route idRoute
        compile $ do
             
            posts <- constField "posts" <$> postList pattern unifiedExamCtx recentFirst "tagItem"
            let ctxx = mconcat  [constField "title" title , (makeDefaultCtx year unifiedExamTags) ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"        posts
                >>= loadAndApplyTemplate "templates/unifiedexams.html" ctxx
                >>= loadAndApplyTemplate "templates/base.html"         ctxx
                >>= relativizeUrls

    -- news tags
    tagsRules newsTags $ \tag pattern -> do
        let title = "Էջեր պիտակված՝ &#8216;" ++ tag ++ "&#8217;"
        route idRoute
        compile $ do
             
            posts <- constField "posts" <$> postList pattern newsCtx recentFirst "tagItem"
            let ctxx = mconcat  [constField "title" title , (makeDefaultCtx year newsTags) ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" posts
                >>= loadAndApplyTemplate "templates/index.html" ctxx
                >>= loadAndApplyTemplate "templates/base.html"  ctxx
                >>= relativizeUrls


    -- shtemaran tags
    tagsRules shtemaranTags $ \tag pattern -> do
        let title = "Էջեր պիտակված՝ &#8216;" ++ tag ++ "&#8217;"
        route idRoute
        compile $ do
             
            posts <- constField "posts" <$> postList pattern shtemaranCtx recentFirst "tagItem"
            let ctxx = mconcat  [constField "title" title , (makeDefaultCtx year shtemaranTags) ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"     posts
                >>= loadAndApplyTemplate "templates/shtemaran.html" ctxx
                >>= loadAndApplyTemplate "templates/base.html"      ctxx
                >>= relativizeUrls

  
   -- all tags rendeirng
    tagsRules allTags $ \tag pattern -> do
        let title = "From All tagged &#8216;" ++ tag ++ "&#8217;"
        route idRoute
        compile $ do
             
            posts <- constField "posts" <$> postList pattern allTagCtx recentFirst "tagItem"
            let ctxx = mconcat  [constField "title" title , (makeDefaultCtx year allTags) ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" posts
                >>= loadAndApplyTemplate "templates/base.html"  ctxx
                >>= relativizeUrls    


    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "pages/news/*/*.markdown"
            let archiveCtx =
                    listField "posts" baseCtx (return posts) `mappend`
                    constField "title" "News"                `mappend`
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
                    constField "title" "AllTags"         `mappend`
                    tagCloudCtx allTags                  `mappend`
                    tagCloudCtx' "newstagcloud" newsTags `mappend` 
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/allTagsPage.html" xCtx
                >>= loadAndApplyTemplate "templates/base.html" baseCtx 
                >>= relativizeUrls

  {--  match "index.html" $ do
        let title = "Նորություններ"
        route idRoute
        compile $ do
            pp <- postAdvancedCtx newsTags
           -- news <- constField "posts" <$> postTeaserList "pages/news/*/*.markdown" "news-content" pp recentFirst
            news <- constField "posts" <$> postList "pages/news/*/*.markdown" pp recentFirst "post-item"
            let ctxx = mconcat  [constField "title" title , (makeDefaultCtx year newsTags) ]
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html"  news
                >>= loadAndApplyTemplate "templates/index.html"  ctxx
                >>= loadAndApplyTemplate "templates/base.html"  ctxx
                >>= relativizeUrls--}


    let shs = ["armenian", "biology",   "chemistry", "english",
               "france",   "geography", "germany",   "history",
               "math",     "russian" ]

    processShtemaran "armenian"  baseCtx shtemaranTags year
    processShtemaran "biology"   baseCtx shtemaranTags year 
    processShtemaran "chemistry" baseCtx shtemaranTags year
    processShtemaran "english"   baseCtx shtemaranTags year
    processShtemaran "france"    baseCtx shtemaranTags year
    processShtemaran "geography" baseCtx shtemaranTags year
    processShtemaran "germany"   baseCtx shtemaranTags year
    processShtemaran "history"   baseCtx shtemaranTags year
    processShtemaran "math"      baseCtx shtemaranTags year
    processShtemaran "russian"   baseCtx shtemaranTags year

   {-- map (\p ->                  
          --all shtemarans in one page
            match (fromGlob "pages/shtemaran/" ++p ++".markdown") $ do
                route $ customRoute $ (processPagesRoute "shtemaran") .  toFilePath
                compile $ shtemaranCompiler p baseCtx shtemaranTags year
        ) shs      --}
    
    --ToDo: remove <!--more--> markups from content
    create ["rss.xml"] $ do
    route idRoute
    compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots "pages/news/*/*.markdown" "news-content"
        renderRss feedConfiguration feedCtx posts




 -- Generate blog index pages: we need to split the articles, sorted
  -- by publication date, into groups for display across the right
  -- number of index pages.
    mds <- getAllMetadata newsPattern
    let ids = reverse $ map fst mds
        pids = chunk newsPerIndexPage ids
        indexPages =
          map (\i -> fromFilePath $ "index" ++
                     (if i == 1 then "" else show i) ++ ".html")
          [1..length pids]
        indexes = zip indexPages pids
    create (map fst indexes) $ do
      route idRoute
      compile $ do
        pp <- postAdvancedCtx newsTags
        indexCompiler newsTags  pp indexes year

    where
      newsPattern = fromGlob "pages/news/*/*.markdown" 



--------------------------------------------------------------------------------
-- | Split list into equal sized sublists.
--
chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs

-- | String together multiple template compilers.
--
loadAndApplyTemplates :: [String] -> Context String ->
                         Item String -> Compiler (Item String)
loadAndApplyTemplates [c] ctx i =
  loadAndApplyTemplate (fromFilePath $ "templates/" ++ c ++ ".html") ctx i
loadAndApplyTemplates (c:cs) ctx i = do
  i' <- loadAndApplyTemplate (fromFilePath $ "templates/" ++ c ++ ".html") ctx i
  loadAndApplyTemplates cs ctx i'

-- | Index page compiler: generate a single index page based on
-- identifier name, with the appropriate posts on each one.
--
indexCompiler :: Tags -> Context String
                 -> [(Identifier, [Identifier])] -> String -> Compiler (Item String)
indexCompiler tags ctx ids year = do
  pg <- (drop 5 . dropExtension . takeBaseName . toFilePath) <$> getUnderlying
  let i = if pg == "" then 1 else (read pg :: Int)
      n = length ids
      older = indexNavLink i 1 n
      newer = indexNavLink i (-1) n
  list <- postList (fromList $ snd $ ids !! (i - 1))  ctx recentFirst "post-item"
  news <- constField "posts" <$> postTeaserList  (fromList $ snd $ ids !! (i - 1))  "news-content" ctx recentFirst
  makeItem ""
    >>= loadAndApplyTemplate "templates/posts.html"  news
    >>= loadAndApplyTemplates ["page1", "index1", "base"]
         (constField "title" "ԳԹԿ" `mappend`
          constField "pagetitle" "ԳԹԿ" `mappend`
         -- constField "posts" list `mappend`
          constField "navlinkolder" older `mappend`
          constField "navlinknewer" newer `mappend`
          tagCloudCtx tags `mappend`
          yearCtx     year `mappend`
          ctx `mappend`
          defaultContext)
    >>= relativizeUrls


-- | Generate navigation link HTML for stepping between index pages.
--
indexNavLink :: Int -> Int -> Int -> String
indexNavLink n d maxn = renderHtml ref
  where ref = if (refPage == "") then ""
              else H.a ! A.href (toValue $ toUrl $ refPage) $
                   (H.preEscapedToMarkup lab)
        lab :: String
        lab = if (d > 0) then "&laquo; Նախորդ" else "Հաջորդ &raquo;"
        refPage = if (n + d < 1 || n + d > maxn) then ""
                  else case (n + d) of
                    1 -> "/index.html"
                    _ -> "/index" ++ (show $ n + d) ++ ".html"

processPagesRoute :: FilePath -> [Char] -> FilePath
processPagesRoute root path = root </>  year </> (replaceExtension fileName "html")
                                        where fileName =  last (splitDirectories path)
                                              year | length (splitDirectories path) < 4  = ""
                                                   | otherwise = last $ init (splitDirectories path)
                       

processShtemaran :: [Char]  -> Context String -> Tags -> String -> Rules ()
processShtemaran p baseCtx shtemaranTags year = do
           match (fromGlob $ "pages/shtemaran/" ++ p ++".markdown") $ do
                route $ customRoute $ (processPagesRoute "shtemaran") .  toFilePath
                compile $ shtemaranCompiler p baseCtx shtemaranTags year


shtemaranCompiler :: String -> Context String -> Tags -> String -> Compiler (Item String)
shtemaranCompiler subject baseCtx shtemaranTags year = do
            posts2012 <- recentFirst =<< loadAll  (fromGlob $  "pages/shtemaran/2012-2013/" ++ subject ++"*.markdown") 
            posts2013 <- recentFirst =<< loadAll  (fromGlob $  "pages/shtemaran/2013-2014/" ++ subject ++"*.markdown") 
            let archiveCtx =
                    listField "posts2012" baseCtx (return posts2012) `mappend`
                    listField "posts2013" baseCtx (return posts2013) `mappend`
                    constField "title" "Shtemarans"                  `mappend`
                    tagCloudCtx shtemaranTags                        `mappend`
                    tagsField "tags" shtemaranTags                   `mappend` 
                    defaultContext

            makeItem ""
                 >>= loadAndApplyTemplate "templates/shtemaran-list.html" archiveCtx
                 >>= loadAndApplyTemplate "templates/shtemaran.html"   (makeUnifiedCtx shtemaranTags)
                 >>= loadAndApplyTemplate "templates/base.html" (makeBasePageCtx year)
                 >>= relativizeUrls
-- | Full context for posts.
--
postAdvancedCtx :: MonadMetadata m => Tags -> m (Context String)
postAdvancedCtx t = do
  return $
    mapContext prettify
      (tagsFieldWith getTags render join "prettytags" t) `mappend`
    tagCloudCtx t `mappend`
    functionField "readmore" readMoreField `mappend`
    functionField "pagetitle" pageTitle `mappend`
    dateField "date" "%Y-%m-%d" `mappend`
    defaultContext
  where prettify "" = ""
        prettify s = "<div class=\"tags\">" ++ s ++ "</div>"
        render _ Nothing = Nothing
        render tag (Just filePath) = Just $ H.span ! A.class_ "tag" $
          H.a ! A.href (toValue $ toUrl filePath) $ H.toHtml tag
        join = mconcat . intersperse " "
        pageTitle _ i = do
          m <- getMetadata $ itemIdentifier i
          return $ "gtk.am | " ++ (m M.! "title")


contestAdvancedCtx :: MonadMetadata m => Tags -> m (Context String)
contestAdvancedCtx t = do
  return $
    mapContext prettify
      (tagsFieldWith getTags render join "prettytags" t) `mappend`
    tagsField "tags" t `mappend`
    defaultContext
  where prettify "" = ""
        prettify s = "<div class=\"tags\">" ++ s ++ "</div>"
        render _ Nothing = Nothing
        render tag (Just filePath) = Just $ H.span ! A.class_ "tag" $
          H.a ! A.href (toValue $ toUrl filePath) $ H.toHtml tag
        join = mconcat . intersperse " "
       

-- | Generate "Read more" link for an index entry.
--
readMoreField :: [String] -> Item String -> Compiler String
readMoreField _ i = do
  rte <- getRoute $ itemIdentifier i
  return $ case rte of
    Nothing -> ""
    Just r -> if isInfixOf "<!--MORE-->" (itemBody i)
              then readMoreLink r
              else ""
    where readMoreLink r' =
            renderHtml $ H.div ! A.class_ "readmore" $
            H.a ! A.href (toValue $ "/" ++ r') $
            H.preEscapedToMarkup ("Read more &raquo;"::String)


-- | Creates a "year" context from a string representation of the current year
yearCtx :: String -> Context String
yearCtx year = field "year" $ \item -> return year

-- | Given a collection of Tags, builds a context with a rendered tag cloud
{--tagCloudCtx :: Tags -> Context String
tagCloudCtx tags = field "tagcloud" $ \item -> rendered
  where rendered = renderTagCloud 85.0 165.0 tags--}

tagCloudCtx' :: String -> Tags -> Context String
tagCloudCtx' tagHolderName = tagCloudFieldWith tagHolderName  makeLink (intercalate " ") 50 135
  where
    makeLink minSize maxSize tag url count min' max' = renderHtml $
        H.span ! A.class_ "tagcloud" !
        A.style (toValue $ "font-size: " ++ size count min' max') $
        H.a ! A.href (toValue url) $ H.toHtml tag
      where
        -- Show the relative size of one 'count' in percent
        size count min' max' =
          let diff = 1 + fromIntegral max' - fromIntegral min'
              relative = (fromIntegral count - fromIntegral min') / diff
              size' = floor $ minSize + relative * (maxSize - minSize)
          in show (size' :: Int) ++ "%"


tagCloudCtx :: Tags -> Context String
tagCloudCtx = tagCloudCtx' "tagcloud"

tagsOnPageCtx :: Tags -> Context String
tagsOnPageCtx = tagCloudCtx' "tags"
        

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
    dateField "date"  "%Y-%m-%d"  `mappend`
    constField "title" "tt" `mappend`
    defaultContext

-- | Creates the default post context used by pages with posts/post listings
defaultPostCtx :: Tags -> Context String
defaultPostCtx tags = mconcat
  [ dateField "date"  "%Y-%m-%d"
  , tagsField "tags" tags
  , defaultContext
  ]

postCtx1 :: Context String
postCtx1 =
      constField "mainselection" " "    `mappend`
      constField "aboutselection" " "   `mappend`
      constField "contactselection" " " `mappend`
      constField "archiveselection" " " `mappend`
      defaultContext


--context for archive, or any generated file to process with default.hetml temaplate
indexarchiveCtx :: String -> [Item String] -> Context String
indexarchiveCtx postToken ps =
        listField postToken  postCtx (return ps)         `mappend`
        constField "title" "Archive"                     `mappend`
        constField "mainselection" " "                   `mappend`
        constField "aboutselection" " "                  `mappend`
        constField "contactselection" " "                `mappend`
        constField "archiveselection" "class='selected'" `mappend`
        defaultContext
        
mainindexCtx :: String -> [Item String] -> Context String
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
         -> String
         -> Compiler String
postList pattern postCtx sortFilter tmpl = do
    posts   <- sortFilter =<< loadAll pattern
    itemTpl <- loadBody $ fromFilePath $ "templates/" ++ tmpl ++ ".html"
    applyTemplateList itemTpl  postCtx posts
    --applyTemplateList itemTpl (teaserField "teaser" "news-content" `mappend` postCtx) posts


postTeaserList :: Pattern
               -> String   
               -> Context String
               -> ([Item String] -> Compiler [Item String])
               -> Compiler String
postTeaserList pattern teaserSnapshot postCtx sortFilter =
    postList pattern  (teaserField "teaser" teaserSnapshot `mappend` postCtx) sortFilter "post-item"
    




-- | RSS feed configuration.
--
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "gtk.am RSS feed."
    , feedDescription = "RSS feed for gtk.am"
    , feedAuthorName  = "Arthur"
    , feedAuthorEmail = "info@gtk.am"
    , feedRoot        = "http://gtk.am"
    }


dateTranslation s  = unwords $  (monthToArm x) : xs
   where
     (x:xs) =  words s

monthToArm :: String -> String
monthToArm "December" = "Դեկտեմբեր"
monthToArm x = x

armToLat :: Char -> [Char]

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



