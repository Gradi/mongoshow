 {-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

module BsonSchemasView
    ( ViewModel(..)
    , render
    ) where

import qualified BsonSchema as BS
import qualified Data.Text as Text
import qualified Data.Time as DT
import qualified Data.Time.Format as DTF
import qualified Database.MongoDB as Mdb
import qualified Data.ByteString as ByteString
import Control.Monad (mapM)
import Control.Monad.Reader
import Data.List (intersperse)
import Data.String (IsString(fromString))
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Utils (groupBy)

data ViewModel = ViewModel {
    localTime :: DT.LocalTime
  , schemas :: [BS.BsonSchema]
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
            H.title $ title

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
        p $ (toHtml ("Date " :: String)) >> toHtml time
        hr
        H.div $ do
            h2 "Contents: "
            toHtml tableOfContents
        hr
        article $ toHtml mainContent

renderTocEntry :: [BS.BsonSchema] -> View
renderTocEntry dbCollections = return $ H.div $ do
    aDb db $ h3 $ dbName db
    toHtml $ intersperse br $ scToHtml <$> dbCollections

    where db = Prelude.head dbCollections
          scToHtml sc = aColl sc $ toHtml (col ++ " (" ++ (show $ BS.count sc) ++ ")")
              where col = Text.unpack $ BS.collection sc

renderSingleDbSection :: [BS.BsonSchema] -> View
renderSingleDbSection dbColls = do
    schemasSection <- mapM renderSchemaSection dbColls >>= return . toHtml
    return $ H.div $ do
        h2 ! A.id (dbId db) $ (dbName db)
        schemasSection
        where db = Prelude.head dbColls

renderSchemaSection :: BS.BsonSchema -> View
renderSchemaSection schema = return $ H.div $ do
    h3 ! A.id (collId schema) $ (collName schema)
    p $ toHtml ("Documents read: " ++ (show $ BS.count schema))
    toHtml $ BS.schema schema
    hr

databaseName :: BS.BsonSchema -> BS.BsonSchema -> Bool
databaseName l r = BS.database l == BS.database r

aDb :: BS.BsonSchema -> Html -> Html
aDb schema html = a ! href (fromString ("#" ++ db)) $ html
    where db = Text.unpack $ BS.database schema

aColl :: BS.BsonSchema -> Html -> Html
aColl schema html = a ! href (fromString ("#" ++ db ++ "." ++ coll)) $ html
    where db = Text.unpack $ BS.database schema
          coll = Text.unpack $ BS.collection schema

dbId :: BS.BsonSchema -> AttributeValue
dbId = fromString . Text.unpack . BS.database

collId :: BS.BsonSchema -> AttributeValue
collId sch = fromString (db ++ "." ++ coll)
    where db = Text.unpack $ BS.database sch
          coll = Text.unpack $ BS.collection sch

dbName :: BS.BsonSchema -> Html
dbName = toHtml . BS.database

collName :: BS.BsonSchema -> Html
collName schema = toHtml (db ++ "." ++ coll)
    where db = Text.unpack $ BS.database schema
          coll = Text.unpack $ BS.collection schema

instance ToMarkup Mdb.Value where
    toMarkup (Mdb.Float val) = toHtml val
    toMarkup (Mdb.String str) = toHtml str
    toMarkup (Mdb.Doc document) = table $ do
        thead $ tr $ th "Key" >> th "Value"
        tbody $ toHtml $ fieldToRow <$> document
        where fieldToRow (label Mdb.:= value) = tr $ td (toHtml label) >> td (toHtml value)
    toMarkup (Mdb.Array values) = H.div $ do
        "["
        toHtml $ intersperse (toHtml (", " :: String)) $ toHtml <$> values
        "]"
    toMarkup (Mdb.Bin _) = "<Binary>"
    toMarkup (Mdb.Fun _) = "<Function>"
    toMarkup (Mdb.Uuid _) = "<Uuid>"
    toMarkup (Mdb.Md5 _) = "<Md5>"
    toMarkup (Mdb.UserDef _) = "<UserDef>"
    toMarkup (Mdb.ObjId objId) = toHtml $ show objId
    toMarkup (Mdb.Bool bool) = toHtml bool
    toMarkup (Mdb.UTC utc) = toHtml utc
    toMarkup (Mdb.Null) = "<Null>"
    toMarkup (Mdb.RegEx (Mdb.Regex text opt)) = toHtml $ (Text.unpack text) ++ " " ++ (Text.unpack opt)
    toMarkup (Mdb.JavaScr (Mdb.Javascript doc text)) = H.div $ do
        p "Environment: "
        toHtml doc
        p "Code: "
        toHtml text
    toMarkup (Mdb.Sym (Mdb.Symbol sym)) = toHtml sym
    toMarkup (Mdb.Int32 int32) = toHtml int32
    toMarkup (Mdb.Int64 int64) = toHtml int64
    toMarkup (Mdb.Stamp (Mdb.MongoStamp int64)) = toHtml int64
    toMarkup (Mdb.MinMax minMax) = toHtml $ show minMax

instance ToMarkup Mdb.Document where
    toMarkup doc = toHtml $ Mdb.Doc doc

timeToMarkup :: (DT.FormatTime t) => t -> Html
timeToMarkup = toHtml . DTF.formatTime DTF.defaultTimeLocale DTF.rfc822DateFormat

instance ToMarkup DT.UTCTime where
    toMarkup = timeToMarkup
instance ToMarkup DT.LocalTime where
    toMarkup = timeToMarkup
