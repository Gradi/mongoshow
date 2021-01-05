{-# LANGUAGE OverloadedStrings #-}

module PrettyPrintBson
    ( prettyPrint
    ) where

import qualified Data.ByteString as ByteString
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Format as TimeFormat
import qualified Database.MongoDB as Mdb
import qualified Numeric as Numeric
import Prettyprinter
import Prettyprinter.Render.Text
import Data.List (intersperse)

prettyPrint :: Mdb.Value -> Text.Text
prettyPrint = renderStrict . layoutPretty defaultLayoutOptions . pretty

instance Pretty Mdb.Value where
    pretty (Mdb.Float val)                        = pretty val
    pretty (Mdb.String val)                       = pretty $ wrap val
    pretty (Mdb.Doc doc)                          =
        pretty '{' <> hardline <>
        indent 4 (hcat $ intersperse (pretty ',' <> hardline) (pretty <$> doc)) <>
        hardline <> pretty '}'
    pretty (Mdb.Array arr)                        =
        pretty '[' <> hardline <>
        indent 4 (hcat $ intersperse (pretty ',' <> hardline) (pretty <$> arr)) <>
        hardline <> pretty ']'
    pretty (Mdb.Bin (Mdb.Binary str))             = pretty str
    pretty (Mdb.Fun (Mdb.Function str))           = pretty str
    pretty (Mdb.Uuid (Mdb.UUID str))              = pretty str
    pretty (Mdb.Md5 (Mdb.MD5 str))                = pretty str
    pretty (Mdb.UserDef (Mdb.UserDefined str))    = pretty str
    pretty (Mdb.ObjId objId)                      = pretty objId
    pretty (Mdb.Bool bool)                        = pretty bool
    pretty (Mdb.UTC utcTime)                      = pretty utcTime
    pretty (Mdb.Null)                             = pretty ("null" :: String )
    pretty (Mdb.RegEx (Mdb.Regex val1 val2))      = pretty $ Text.concat [val1, Text.singleton '/', val2]
    pretty (Mdb.JavaScr (Mdb.Javascript doc src)) = pretty $ Mdb.Doc [ "Environment" Mdb.=: doc, "Source" Mdb.=: src]
    pretty (Mdb.Sym (Mdb.Symbol val))             = pretty $ wrap val
    pretty (Mdb.Int32 val)                        = pretty val
    pretty (Mdb.Int64 val)                        = pretty val
    pretty (Mdb.Stamp (Mdb.MongoStamp val))       = pretty val
    pretty (Mdb.MinMax minMax)                    = pretty $ wrap' $ show minMax

instance Pretty Mdb.Field where
    pretty (label Mdb.:= value) = (pretty $ wrap label) <> pretty (": " :: String ) <> pretty value

instance Pretty ByteString.ByteString where
    pretty = pretty . wrap' . ByteString.foldl' toHex []
        where toHex res byte = res ++ showHex byte

instance Pretty Mdb.ObjectId where
    pretty (Mdb.Oid val1 val2) = pretty $ wrap' $ showHex val1 ++ showHex val2

instance Pretty Time.UTCTime where
    pretty = pretty . wrap' . TimeFormat.formatTime TimeFormat.defaultTimeLocale TimeFormat.rfc822DateFormat

showHex :: (Integral a, Show a) => a -> String
showHex a = Numeric.showHex a ""

wrap :: Text.Text -> Text.Text
wrap text = Text.concat [Text.singleton '"', text, Text.singleton '"']

wrap' :: String -> String
wrap' str = "\"" ++ str ++ "\""
