 {-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module BsonSchemasView
    ( ViewModel(..)
    , render
    ) where

import qualified BsonSchema
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Format as TimeFormat
import qualified Database.MongoDB as Mdb
import qualified PrettyPrintBson
import Control.Monad.Reader
import Data.List (intersperse)
import Data.String (IsString(fromString))
import Utils (groupBy)
import Data.Functor ((<&>))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A


data ViewModel = ViewModel {
    localTime :: Time.LocalTime
  , schemas :: [BsonSchema.BsonSchema]
  , host :: String }

type View = Reader ViewModel Html

render :: ViewModel -> Html
render = runReader render'

render' :: View
render' = do
    head <- renderHead
    body <- renderBody
    return $ docTypeHtml ! lang "en" $ do
        head
        body

renderHead :: View
renderHead = do
    host <- asks host
    let title = toHtml $ "Schemas: " ++ host
    return $
        H.head $ do
            meta ! charset "utf-8"
            highlightjs
            H.title title

renderBody :: View
renderBody = do
    host <- asks host
    time <- asks localTime
    schemas <- asks schemas
    let schemasByDb = groupBy databaseName schemas
    tableOfContents <- mapM renderTocEntry schemasByDb
    mainContent <- mapM renderSingleDbSection schemasByDb

    return $ body $ do
        h1 $ toHtml ("Host " ++ host)
        p $ toHtml ("Date " :: String) >> toHtml time
        hr
        H.div $ do
            h2 "Contents: "
            toHtml tableOfContents
        hr
        article $ toHtml mainContent

renderTocEntry :: [BsonSchema.BsonSchema] -> View
renderTocEntry dbCollections = return $ H.div $ do
    aDb db $ h3 $ dbName db
    toHtml $ intersperse br $ schemaToHtml <$> dbCollections

    where db = Prelude.head dbCollections
          schemaToHtml sc = aColl sc $ do
              toHtml $ BsonSchema.collection sc
              " ("
              toHtml $ BsonSchema.count sc
              ")"

renderSingleDbSection :: [BsonSchema.BsonSchema] -> View
renderSingleDbSection dbColls = do
    schemasSection <- mapM renderSchemaSection dbColls <&> toHtml
    return $ H.div $ do
        h2 ! A.id (dbId db) $ dbName db
        schemasSection
        where db = Prelude.head dbColls

renderSchemaSection :: BsonSchema.BsonSchema -> View
renderSchemaSection schema = return $ H.div $ do
    h3 ! A.id (collId schema) $ collName schema
    p $ do
        "Documents read: "
        toHtml $ BsonSchema.count schema
    toHtml $ BsonSchema.schema schema
    hr

-- Helpers

databaseName :: BsonSchema.BsonSchema -> BsonSchema.BsonSchema -> Bool
databaseName l r = BsonSchema.database l == BsonSchema.database r

aDb :: BsonSchema.BsonSchema -> Html -> Html
aDb schema html = a ! href (fromString ("#" ++ db)) $ html
    where db = Text.unpack $ BsonSchema.database schema

aColl :: BsonSchema.BsonSchema -> Html -> Html
aColl schema html = a ! href (fromString ("#" ++ db ++ "." ++ coll)) $ html
    where db = Text.unpack $ BsonSchema.database schema
          coll = Text.unpack $ BsonSchema.collection schema

dbId :: BsonSchema.BsonSchema -> AttributeValue
dbId = fromString . Text.unpack . BsonSchema.database

collId :: BsonSchema.BsonSchema -> AttributeValue
collId sch = fromString (db ++ "." ++ coll)
    where db = Text.unpack $ BsonSchema.database sch
          coll = Text.unpack $ BsonSchema.collection sch

dbName :: BsonSchema.BsonSchema -> Html
dbName = toHtml . BsonSchema.database

collName :: BsonSchema.BsonSchema -> Html
collName schema = toHtml (db ++ "." ++ coll)
    where db = Text.unpack $ BsonSchema.database schema
          coll = Text.unpack $ BsonSchema.collection schema

highlightjs :: Html
highlightjs = do
    link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.5.0/styles/mono-blue.min.css"
    script ! src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.5.0/highlight.min.js" $ ""
    script "hljs.initHighlightingOnLoad();"

timeToMarkup :: (Time.FormatTime t) => t -> Html
timeToMarkup = toHtml . TimeFormat.formatTime TimeFormat.defaultTimeLocale TimeFormat.rfc822DateFormat

instance ToMarkup Time.UTCTime where
    toMarkup = timeToMarkup
instance ToMarkup Time.LocalTime where
    toMarkup = timeToMarkup

instance ToMarkup Mdb.Value where
    toMarkup value = pre $ code ! class_ "json" $ toHtml $ PrettyPrintBson.prettyPrint value

instance ToMarkup Mdb.Document where
    toMarkup document = toHtml $ Mdb.Doc document
