{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Text.Pandoc

main :: IO ()
main = hakyllWith configuration $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith defaultHakyllReaderOptions myHakyllWriterOptions
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

    match "about.html" $ do
        route idRoute
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderRss myFeedConfiguration feedCtx posts


configuration :: Configuration
configuration = defaultConfiguration
    { deployCommand = "./deploy.sh" }

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext


postsCompiler :: [Item String] -> Compiler (Item String)
postsCompiler posts = do
    let postsCtx =
            listField "posts" postCtx (return posts) <>
            defaultContext

    getResourceBody
        >>= applyAsTemplate postsCtx
        >>= loadAndApplyTemplate "templates/default.html" postsCtx
        >>= relativizeUrls


myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "mapisto"
    , feedDescription = "blog"
    , feedAuthorName  = "Johanna Jokio"
    , feedAuthorEmail = ""
    , feedRoot        = "https://johannajokio.github.io/rss.xml"
    }


myHakyllWriterOptions :: WriterOptions
myHakyllWriterOptions = defaultHakyllWriterOptions
    { writerHTMLMathMethod = MathML }

