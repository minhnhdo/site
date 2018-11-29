--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import       Control.Monad (liftM)
import       Data.Monoid (mappend)
import       Data.Text.Lazy (unpack)
import       Clay (Css, compact, renderWith)
import       Hakyll

import       DefaultCss (defaultCss)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "resume.pdf" $ do
    route   idRoute
    compile copyFileCompiler

  create ["CNAME"] $ do
    route idRoute
    compile $ makeItem ("minhdo.org" :: String)

  create ["css/default.css"] $ do
    route idRoute
    compile $ compileWithClay defaultCss

  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  match "posts/*.mkd" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "_templates/post.html" (postCtxWithTags tags)
      >>= loadAndApplyTemplate "_templates/default.html" postCtx
      >>= relativizeUrls

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title `mappend`
                listField "posts" postCtx (return posts) `mappend`
                defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "_templates/tag.html" ctx
        >>= loadAndApplyTemplate "_templates/default.html" ctx
        >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx =
            listField "posts" postCtx allPosts `mappend`
            constField "title" "Archives"      `mappend`
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "_templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "_templates/default.html" archiveCtx
        >>= relativizeUrls


  match "index.mkd" $ do
    route   $ setExtension "html"
    compile $ do
      let indexCtx =
            listField "posts" postCtx (liftM (take 5) allPosts) `mappend`
            defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= renderPandoc
        >>= loadAndApplyTemplate "_templates/default.html" indexCtx
        >>= relativizeUrls

  match "_templates/*.html" $ compile templateCompiler

--------------------------------------------------------------------------------
allPosts :: Compiler [Item String]
allPosts = recentFirst =<< loadAll "posts/*"

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` defaultContext

compileWithClay :: Css -> Compiler (Item String)
compileWithClay = makeItem . unpack . renderWith compact []
