{-# LANGUAGE OverloadedStrings #-}

module BsonValueFormatting
    ( format
    ) where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Format as TimeFormat
import qualified Database.MongoDB as Mdb
import qualified Numeric as Numeric
import Control.Monad.Reader
import Data.List (intersperse)

type FormatContext = Reader FormatOptions String

data FormatOptions = FormatOptions {
    indentation :: Int
    }

format :: Mdb.Value -> String
format value = runReader (format' value) FormatOptions { indentation = 0 }

format' :: Mdb.Value -> FormatContext
format' (Mdb.Float val)                        = return $ show val
format' (Mdb.String val)                       = return $ wrap $ Text.unpack val
format' (Mdb.Doc doc)                          = incIndentation $ formatDocument doc
format' (Mdb.Array arr)                        = incIndentation $ formatArray arr
format' (Mdb.Bin (Mdb.Binary str))             = formatByteString str
format' (Mdb.Fun (Mdb.Function str))           = formatByteString str
format' (Mdb.Uuid (Mdb.UUID str))              = formatByteString str
format' (Mdb.Md5 (Mdb.MD5 str))                = formatByteString str
format' (Mdb.UserDef (Mdb.UserDefined str))    = formatByteString str
format' (Mdb.ObjId value)                      = formatObjId value
format' (Mdb.Bool bool)                        = return $ show bool
format' (Mdb.UTC utcTime)                      = formatTime utcTime
format' (Mdb.Null)                             = return "null"
format' (Mdb.RegEx (Mdb.Regex val1 val2))      = return $ Text.unpack val1 ++ "/" ++ Text.unpack val2
format' (Mdb.JavaScr (Mdb.Javascript doc src)) = incIndentation $ formatDocument [ "Source" Mdb.=: src, "Environment" Mdb.=: doc ]
format' (Mdb.Sym (Mdb.Symbol str))             = return $ wrap $  Text.unpack str
format' (Mdb.Int32 val)                        = return $ show val
format' (Mdb.Int64 val)                        = return $ show val
format' (Mdb.Stamp (Mdb.MongoStamp val))       = return $ show val
format' (Mdb.MinMax minmax)                    = return $ wrap $ show minmax

incIndentation :: FormatContext -> FormatContext
incIndentation context = withReader incrementIndentation context
    where incrementIndentation cur = cur { indentation = indentation cur + 1 }

formatDocument :: Mdb.Document -> FormatContext
formatDocument doc = do
    prefix <- getPrefix
    fields <- mapM formatField doc
    let fields' = concat $ intersperse ",\r\n" fields
    return $ "{\r\n" ++ fields' ++"\r\n" ++ prefix ++ "}"

formatArray :: [Mdb.Value] -> FormatContext
formatArray xs = do
    values <- mapM format' xs
    let values' = concat $ intersperse ",\r\n" values
    return $ "[\r\n" ++ values' ++ "\r\n]"

formatByteString :: ByteString.ByteString -> FormatContext
formatByteString str = return $ wrap $ ByteString.foldl' wordToHex [] str
    where wordToHex result word = result ++ showHex word

formatObjId :: Mdb.ObjectId -> FormatContext
formatObjId (Mdb.Oid val1 val2) = return $ wrap $ (showHex val1 ++ showHex val2)

formatTime :: (Time.FormatTime t) => t -> FormatContext
formatTime = return . wrap . TimeFormat.formatTime TimeFormat.defaultTimeLocale TimeFormat.rfc822DateFormat

getPrefix :: FormatContext
getPrefix = do
    indentation <- asks indentation
    return $ concat $ take indentation $ repeat "    "

formatField :: Mdb.Field -> FormatContext
formatField (label Mdb.:= value) = do
    prefix <- getPrefix
    value' <- format' value
    return $ prefix ++ wrap (Text.unpack label) ++ " : " ++ value'

showHex :: (Integral a, Show a) => a -> String
showHex a = Numeric.showHex a ""

wrap :: String -> String
wrap a = "\"" ++ a ++ "\""
