module Main where

import qualified BsonSchema as BsonSchema
import qualified BsonSchemasView as View
import qualified CommandLine as CL
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Time as Time
import Database.MongoDB
import Text.Blaze.Html.Renderer.Utf8
import Control.Monad.IO.Class (liftIO)


main :: IO ()
main = CL.execParser CL.programInfo >>= CL.execCommandLineContext newMain

newMain :: CL.CommandLineContext IO ()
newMain = do
    host <- CL.askHost
    outFile <- CL.askOutFile

    pipe <- liftIO $ connect $ readHostPort host
    dbs <- access pipe master emptyDb allDatabases >>= restrictDbs
    schemas <- mapM (analyzeDatabase pipe) dbs >>= return . concat

    localTime <- liftIO $ Time.getZonedTime >>= return . Time.zonedTimeToLocalTime
    let html = renderHtml $ View.render View.ViewModel { View.localTime = localTime, View.schemas = schemas, View.host = host }

    liftIO $ ByteString.writeFile outFile html
    liftIO $ close pipe

    return ()


analyzeDatabase :: Pipe -> Database -> CL.CommandLineContext IO [BsonSchema.BsonSchema]
analyzeDatabase pipe database = do
    collections <- access pipe master database allCollections >>= restrictCollections
    mapM (analyzeCollection pipe database) collections

analyzeCollection :: Pipe -> Database -> Collection -> CL.CommandLineContext IO BsonSchema.BsonSchema
analyzeCollection pipe database collection = do
    limit <- CL.askLimit
    let filter = (select [] collection) { limit = fromIntegral limit }
    documents <- access pipe master database $ find filter >>= rest
    return $ BsonSchema.generateSchema database collection documents


restrictDbs :: (Monad m) => [Database] -> CL.CommandLineContext m [Database]
restrictDbs dbs = do
    maybeAllowedDatabases <- CL.askDatabases
    let maybeAllowedDatabases' = maybeAllowedDatabases >>= return . (map Text.pack)
    return $ maybeIntersects dbs maybeAllowedDatabases'

restrictCollections :: (Monad m) => [Collection] -> CL.CommandLineContext m [Collection]
restrictCollections colls = do
    maybeAllowedCollections <- CL.askCollections
    let maybeAllowedCollections' = maybeAllowedCollections >>= return . (map Text.pack)
    return $ maybeIntersects colls maybeAllowedCollections'

maybeIntersects :: (Eq a) => [a] -> Maybe [a] -> [a]
maybeIntersects list maybeList = case maybeList of
    Nothing -> list
    Just secondList -> filter (`elem` secondList) list

emptyDb :: Database
emptyDb = Text.empty
