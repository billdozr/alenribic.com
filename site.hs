{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import System.FilePath (replaceExtension)
import Text.Pandoc.Options

main :: IO ()
main = hakyllWith config $ do
    -- Copy images
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Render each and every post
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls

    -- Post list
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let ctx = constField "title" "All posts" `mappend`
                      listField "posts" (postCtx tags) (return posts) `mappend`
                      defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Index
    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtx tags) (return posts) `mappend`
                    field "tagcloud" (\_ -> renderTagCloud 100 120 tags) `mappend`
                    constField "title" "notes from a local reference frame" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    -- Bookshelf page
    match "bookshelf.markdown" $ do
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx tags `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedCtx posts

    -- Render sitemap
    create ["sitemap.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            let root = feedRoot feedConfiguration
                staticPages =
                    [ root ++ "/"
                    , root ++ "/posts.html"
                    , root ++ "/bookshelf.html"
                    ]
                sitemapCtx =
                    listField "entries" sitemapEntryCtx (return posts) `mappend`
                    listField "pages" defaultContext
                        (mapM makeItem staticPages) `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

    -- Read templates
    match "templates/*" $ compile templateBodyCompiler

    -- Render tags
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title `mappend`
                      listField "posts" (postCtx tags) (return posts) `mappend`
                      defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    -- Copy presentation files
    match "presentations/**" $ do
        route idRoute
        compile copyFileCompiler

    -- Copy letter files (PDFs)
    match "letters/**" $ do
        route idRoute
        compile copyFileCompiler

    -- Copy robots.txt
    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    -- Render 404 page
    match "404.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

-- Configuration
config :: Configuration
config = defaultConfiguration

-- Contexts
postCtx :: Tags -> Context String
postCtx tags =
    dateField "date" "%B %e, %Y" `mappend`
    tagsField "prettytags" tags `mappend`
    defaultContext

sitemapEntryCtx :: Context String
sitemapEntryCtx =
    constField "root" (feedRoot feedConfiguration) `mappend`
    field "url" (\item -> do
        let identifier = itemIdentifier item
        let path = toFilePath identifier
        let htmlPath = replaceExtension path "html"
        return $ feedRoot feedConfiguration ++ "/" ++ htmlPath) `mappend`
    defaultContext

-- Pandoc compiler with math support
pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let mathExtensions = extensionsFromList
            [ Ext_tex_math_dollars
            , Ext_tex_math_double_backslash
            , Ext_latex_macros
            ]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = defaultExtensions <> mathExtensions
        writerOptions = defaultHakyllWriterOptions
            { writerExtensions = newExtensions
            , writerHTMLMathMethod = MathJax ""
            }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions

-- Feed configuration
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Alen Ribic - notes from a local reference frame"
    , feedDescription = "builder, tech entrepreneur, math & physics major"
    , feedAuthorName  = "Alen Ribic"
    , feedAuthorEmail = "alen.ribic@gmail.com"
    , feedRoot        = "http://alenribic.com"
    }
