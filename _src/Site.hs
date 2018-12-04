--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Clay (Css, compact, renderWith)
import           Control.Monad (liftM)
import           Data.Monoid ((<>))
import           Data.Text.Lazy (unpack)
import qualified Text.Pandoc as P
import           Hakyll

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

  create ["posts/index.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx = listField "posts" postCtx allPosts
                    <> constField "title" "Posts"
                    <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "_templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "_templates/default.html" archiveCtx
        >>= relativizeUrls

  match cheatSheetsPattern $ do
    route $ setExtension "html"
    compile $ pandocLaTexCompiler
          >>= loadAndApplyTemplate "_templates/default.html" defaultContext
          >>= relativizeUrls

  match "notes.mkd" $ do
    route $ setExtension "html"
    compile $ do
      let notesCtx = listField "cheatSheets" defaultContext (loadAll cheatSheetsPattern)
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
postsPattern, cheatSheetsPattern :: Pattern
postsPattern = "posts/*.mkd"
cheatSheetsPattern = "notes/cheat-sheets/*.tex"

allPosts :: Compiler [Item String]
allPosts = recentFirst =<< loadAll postsPattern

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> defaultContext

compileWithClay :: Css -> Compiler (Item String)
compileWithClay = makeItem . unpack . renderWith compact []

pandocLaTexCompiler :: Compiler (Item String)
pandocLaTexCompiler =
  pandocCompilerWith modifiedReaderOptions defaultHakyllWriterOptions
  where modifiedReaderOptions :: P.ReaderOptions
        modifiedReaderOptions = defaultHakyllReaderOptions {
          P.readerExtensions = P.extensionsFromList
                             [ P.Ext_old_dashes
                             , P.Ext_smart
                             , P.Ext_raw_html
                             , P.Ext_auto_identifiers ],
          P.readerStandalone = True
        }
