--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.Maybe (fromMaybe)
import           Data.Char (toUpper, toLower)
import           Hakyll
import           Hakyll.Core.Configuration (Configuration)
import           Data.List (isPrefixOf)
import           System.FilePath (takeFileName)
import           System.Process (system)

--------------------------------------------------------------------------------

spicyDir = "spicyorangeco.github.io"
spicyCache = "_cache"
spicyTmp = spicyCache ++ "/_tmp"
deploy = "cd " ++ spicyDir ++ " && git add --all && git commit -m \"Deploying pages\" && git push origin HEAD:master"

spicyConfig = Configuration
  { destinationDirectory = spicyDir
  , storeDirectory = spicyCache
  , tmpDirectory = spicyTmp
  , providerDirectory = "."
  , ignoreFile = ignoreFile'
  , deployCommand = deploy
  , deploySite = system . deployCommand
  , inMemoryCache = True
  , previewHost = "127.0.0.1"
  , previewPort = 8000
  }
  where
    ignoreFile' path
      | "." `isPrefixOf` fileName = True
      | "#" `isPrefixOf` fileName = True
      | "." `isPrefixOf` fileName = True
      | "." `isPrefixOf` fileName = True
      | otherwise = False
      where
        fileName = takeFileName path
                                                

main :: IO ()
main = hakyllWith spicyConfig $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "js/*" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList ["contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "shop/*" $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/product.html" prodCtx
        >>= loadAndApplyTemplate "templates/productdefault.html" prodCtx
        >>= relativizeUrls

    create ["shop.html"] $ do
      route idRoute
      compile $ do
        products <- loadAll "shop/*"
        let productCtx =
              listField "products" prodCtx (return products) `mappend`
              constField "title" "All Products"              `mappend`
              defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/products.html" productCtx
          >>= loadAndApplyTemplate "templates/productdefault.html" productCtx
          >>= relativizeUrls
          

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

--------------------------------------------------------------------------------
prodCtx :: Context String
prodCtx = mconcat
  [ productContextGen "name"
  , productContextGen "description"
  , productContextGen "price"
  , productContextGen "picture"
  , defaultContext
  ]

productContextGen :: String -> Context String
productContextGen key = field ("product"++formalKey) $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ fromMaybe ("Spicy "++formalKey) $ lookupString ("product-"++key) metadata
  where
    formalKey = capitalize key

capitalize :: String -> String
capitalize (f:r) = toUpper f : map toLower r
capitalize [] = []
