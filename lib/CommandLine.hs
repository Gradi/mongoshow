module CommandLine
    ( CommandLineContext
    , CommandLine(..)
    , commandLineParser
    , programInfo
    , execCommandLineContext

    , askHost
    , askOutFile
    , askDatabases
    , askCollections
    , askLimit

    , execParser
    ) where

import Options.Applicative
import Control.Monad.Reader

type CommandLineContext = ReaderT CommandLine

data CommandLine = CommandLine {
    host :: String
  , outFile :: String
  , databases :: Maybe [String]
  , collections :: Maybe [String]
  , limit :: Int } deriving (Show)

commandLineParser :: Parser CommandLine
commandLineParser = CommandLine
    <$> strOption (
        long "host"
     <> short 'h'
     <> help "MongoDB host"
     <> value "localhost:27017"
     <> showDefault
     )
    <*> strOption (long "out" <> short 'o' <> help "Out file" <> value "schemes.html" <> showDefault)
    <*> maybeMany (strOption (
        long "db"
     <> help "Databases to scan. You may specify this option several times. If not set then all dbs will be scanned."
      ))
    <*> maybeMany (strOption (
        long "collection"
     <> short 'c'
     <> help "Collections to scan. You may specify this option several times. If not set then all collections will be scanned."
     ))
    <*> option auto (
        long "limit"
     <> help "Limits number of bson documents to read. If not set then all documents will be scanned."
     <> value 0
     <> showDefault)

programInfo :: ParserInfo CommandLine
programInfo = info (commandLineParser <**> helper) (
    fullDesc <>
    progDesc "This program scans documents in MongoDB's collections and generates html page with schemes."
    )

askHost :: (Monad m) => CommandLineContext m String
askHost = asks host

askOutFile :: (Monad m) => CommandLineContext m String
askOutFile = asks outFile

askDatabases :: (Monad m) => CommandLineContext m (Maybe [String])
askDatabases = asks databases

askCollections :: (Monad m) => CommandLineContext m (Maybe [String])
askCollections = asks collections

askLimit :: (Monad m) => CommandLineContext m Int
askLimit = do
    limit <- asks limit
    return (if limit < 0 then 0 else limit)

execCommandLineContext :: CommandLineContext m a -> CommandLine -> m a
execCommandLineContext = runReaderT

maybeMany :: (Alternative f) => f a -> f (Maybe [a])
maybeMany f = maybeList <$> many f
    where maybeList [] = Nothing
          maybeList xs = Just xs
