{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mempty, mconcat)

import Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/post.html"
            >>> arr (copyBodyToField "description")
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Render posts list
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "All posts")
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    match "templates/*" $ compile templateCompiler
    
    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
        >>> requireAllA "posts/*" (id *** arr (take 5 . reverse . chronological) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    match (list ["bookshelf.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Render RSS feed
    match  "rss.xml" $ route idRoute
    create "rss.xml" $
        requireAll_ "posts/*" >>> renderRss feedConfiguration

    -- Tags
    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))

    -- Copy files (deep)
    match "presentations/**" $ do
        route idRoute
        compile copyFileCompiler

    match "letters/**" $ do
        route idRoute
        compile copyFileCompiler

    -- Robots file for the crawler
    match "robots.txt" $ do
        route idRoute
        compile copyFileCompiler

    -- Render the 404 page
    match "404.html" $ do
        route idRoute
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
  where
    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 120

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "Alen Ribic"
    , feedDescription = "software engineer, entrepreneur, husband and dad"
    , feedAuthorName  = "Alen Ribic"
    , feedAuthorEmail = "alen.ribic@gmail.com"
    , feedRoot        = "http://alenribic.com"
    }

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler
