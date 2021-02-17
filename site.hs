{-# LANGUAGE OverloadedStrings #-}
import Hakyll

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    match "index.html" $ do
        route idRoute
        compile $ loadAllSnapshots "posts/*" "content"
            >>= fmap (take 3) . recentFirst
            >>= postsCompiler

    match "archive.html" $ do
        route idRoute
        compile $ loadAll "posts/*"
            >>= recentFirst
            >>= postsCompiler

    match "contact.html" $ do
        route $ setExtension "html"
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext


postsCompiler posts = do
    let postsCtx =
            listField "posts" postCtx (return posts) <>
            defaultContext

    getResourceBody
        >>= applyAsTemplate postsCtx
        >>= loadAndApplyTemplate "templates/default.html" postsCtx
        >>= relativizeUrls

