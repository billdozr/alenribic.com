{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll

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
        compile $ pandocCompiler
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
                    constField "title" "Home" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    -- Bookshelf page
    match "bookshelf.markdown" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx tags `mappend` bodyField "description"
            posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
            renderRss feedConfiguration feedCtx posts

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

-- Feed configuration
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Alen Ribic"
    , feedDescription = "software engineer, entrepreneur, husband and dad"
    , feedAuthorName  = "Alen Ribic"
    , feedAuthorEmail = "alen.ribic@gmail.com"
    , feedRoot        = "http://alenribic.com"
    }
