--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import           Clay (Css, compact, renderWith)
import           Control.Monad (liftM)
import           Data.Monoid ((<>))
import           Data.Text.Lazy (unpack)
import qualified Text.Pandoc as P
import           Hakyll

import           SiteCss (siteCss)


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
    compile $ pandocCompilerWith defaultHakyllReaderOptions writerOptions
          >>= loadAndApplyTemplate "_templates/comment-section.html" defaultContext
          >>= loadAndApplyTemplate "_templates/post.html" (postCtxWithTags tags)
          >>= loadAndApplyTemplate "_templates/default.html" postCtx
          >>= relativizeUrls

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title
             <> listField "posts" postCtx (pure posts)
             <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "_templates/tag.html" ctx
        >>= loadAndApplyTemplate "_templates/default.html" ctx
        >>= relativizeUrls

  create ["posts/index.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx = constField "title" "Posts"
                    <> listField "posts" postCtx allPosts
                    <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "_templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "_templates/default.html" archiveCtx
        >>= relativizeUrls

  match coursesPattern $ do
    route $ setExtension "html"
    compile $ pandocCompilerWith defaultHakyllReaderOptions writerOptions
          >>= loadAndApplyTemplate "_templates/course.html" defaultContext
          >>= loadAndApplyTemplate "_templates/comment-section.html" defaultContext
          >>= loadAndApplyTemplate "_templates/default.html" defaultContext
          >>= relativizeUrls

  match cheatSheetsPattern $ do
    route $ setExtension "html"
    compile $ pandocLaTexCompiler
          >>= loadAndApplyTemplate "_templates/default.html" defaultContext
          >>= relativizeUrls

  match compiledCheatSheetsPattern $ do
    route idRoute
    compile copyFileCompiler

  create ["notes/index.html"] $ do
    route $ setExtension "html"
    compile $ do
      let notesCtx = constField "title" "Notes"
                  <> listField "courses" defaultContext (loadAll coursesPattern)
                  <> listField "cheatSheets" cheatSheetsCtx (loadAll cheatSheetsPattern)
                  <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "_templates/note-list.html" notesCtx
        >>= loadAndApplyTemplate "_templates/default.html" notesCtx
        >>= relativizeUrls

  match "index.mkd" $ do
    route $ setExtension "html"
    compile $ do
      let indexCtx = listField "posts" postCtx (liftM (take 5) allPosts)
                  <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= renderPandocWith defaultHakyllReaderOptions writerOptions
        >>= loadAndApplyTemplate "_templates/default.html" indexCtx
        >>= relativizeUrls

  match "_templates/*.html" $ compile templateCompiler

--------------------------------------------------------------------------------
writerOptions :: P.WriterOptions
writerOptions = defaultHakyllWriterOptions {
  P.writerExtensions = newExtensions,
  P.writerHTMLMathMethod = P.MathJax ""
}
  where mathExtensions = [ P.Ext_tex_math_dollars
                         , P.Ext_tex_math_double_backslash
                         , P.Ext_latex_macros ]
        defaultWriterExtensions = P.writerExtensions defaultHakyllWriterOptions
        newExtensions = foldr P.enableExtension defaultWriterExtensions mathExtensions

postsPattern, coursesPattern, cheatSheetsPattern, compiledCheatSheetsPattern :: Pattern
postsPattern = "posts/*.mkd"
coursesPattern = "notes/courses/*/notes.mkd"
cheatSheetsPattern = "notes/cheat-sheets/*.tex"
compiledCheatSheetsPattern = "notes/cheat-sheets/*.pdf"

allPosts :: Compiler [Item String]
allPosts = recentFirst =<< loadAll postsPattern

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

cheatSheetsCtx :: Context String
cheatSheetsCtx = field "pdfUrl" ( pure
                                . toUrl
                                . replaceAll ".tex" (const ".pdf")
                                . toFilePath
                                . itemIdentifier )
              <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> defaultContext

compileWithClay :: Css -> Compiler (Item String)
compileWithClay = makeItem . unpack . renderWith compact []

pandocLaTexCompiler :: Compiler (Item String)
pandocLaTexCompiler = pandocCompilerWith modifiedReaderOptions writerOptions
  where modifiedReaderOptions :: P.ReaderOptions
        modifiedReaderOptions = defaultHakyllReaderOptions {
          P.readerExtensions = P.extensionsFromList
                             [ P.Ext_old_dashes
                             , P.Ext_smart
                             , P.Ext_raw_html
                             , P.Ext_auto_identifiers ],
          P.readerStandalone = True
        }
