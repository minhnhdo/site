--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import       Control.Monad (liftM)
import       Data.Monoid ((<>))
import       Data.Text.Lazy (unpack)
import       Clay (Css, compact, renderWith)
import       Hakyll

import       SiteCss (siteCss)


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "resume.pdf" $ do
    route idRoute
    compile copyFileCompiler

  create ["CNAME"] $ do
    route idRoute
    compile $ makeItem ("minhdo.org" :: String)

  create ["css/site.css"] $ do
    route idRoute
    compile $ compileWithClay siteCss

  tags <- buildTags postsPattern (fromCapture "tags/*.html")

  match postsPattern $ do
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
      let ctx = constField "title" title
             <> listField "posts" postCtx (return posts)
             <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "_templates/tag.html" ctx
        >>= loadAndApplyTemplate "_templates/default.html" ctx
        >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx = listField "posts" postCtx allPosts
                    <> constField "title" "Archive"
                    <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "_templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "_templates/default.html" archiveCtx
        >>= relativizeUrls

  match coursesPattern $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "_templates/default.html" defaultContext
          >>= relativizeUrls

  match "notes.mkd" $ do
    route $ setExtension "html"
    compile $ do
      let notesCtx = listField "courses" defaultContext (loadAll coursesPattern)
                  <> defaultContext

      getResourceBody
        >>= applyAsTemplate notesCtx
        >>= renderPandoc
        >>= loadAndApplyTemplate "_templates/default.html" notesCtx
        >>= relativizeUrls

  match "index.mkd" $ do
    route $ setExtension "html"
    compile $ do
      let indexCtx = listField "posts" postCtx (liftM (take 5) allPosts)
                  <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= renderPandoc
        >>= loadAndApplyTemplate "_templates/default.html" indexCtx
        >>= relativizeUrls

  match "_templates/*.html" $ compile templateCompiler

--------------------------------------------------------------------------------
postsPattern, coursesPattern :: Pattern
postsPattern = "posts/*.mkd"
coursesPattern = "notes/courses/*/notes.mkd"

allPosts :: Compiler [Item String]
allPosts = recentFirst =<< loadAll postsPattern

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> defaultContext

compileWithClay :: Css -> Compiler (Item String)
compileWithClay = makeItem . unpack . renderWith compact []
