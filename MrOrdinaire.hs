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
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  match "resume.pdf" $ do
    route   idRoute
    compile copyFileCompiler

  create ["CNAME"] $ do
    route idRoute
    compile $ makeItem ("minhdo.org" :: String)

  create ["css/default.css"] $ do
    route idRoute
    compile $ compileWithClay defaultCss

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"  postCtx
      >>= loadAndApplyTemplate "templates/default.html" postCtx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx =
            listField "posts" postCtx allPosts `mappend`
            constField "title" "Archives"      `mappend`
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls


  match "index.mkd" $ do
    route   $ setExtension "html"
    compile $ do
      let indexCtx =
            listField "posts" postCtx (liftM (take 5) allPosts) `mappend`
            defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= compileWithPandoc
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
allPosts :: Compiler [Item String]
allPosts = recentFirst =<< loadAll "posts/*"

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext

compileWithPandoc :: Item String -> Compiler (Item String)
compileWithPandoc = return . writePandoc . readPandoc

compileWithClay :: Css -> Compiler (Item String)
compileWithClay = makeItem . unpack . renderWith compact []
