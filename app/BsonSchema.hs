{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns, OverloadedStrings, DeriveDataTypeable #-}

module BsonSchema
    ( BsonSchema(..)
    , generateSchema
    ) where

import qualified Data.ByteString as ByteStr
import qualified Data.Data as Data
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Time.Clock as Clock
import qualified Database.MongoDB as Mdb
import Data.Int (Int64)
import Data.List (foldl')
import Database.MongoDB ((=:))
import Utils (groupBy, sumBy)

data BsonSchema = BsonSchema {
    database :: Mdb.Database
  , collection :: Mdb.Collection
  , schema :: Mdb.Document
  , count :: Int64 }

-- Schema of single bson value
-- with counter of how much times does it appear in bson document
-- and some other, type specific properties.
-- 'MultiValue' is like array, but it is for case when
-- bsondoc under the same field has different bson value types.
data SValue =
    Float      { count :: Int64, fmin :: Double, fmax :: Double }
  | String     { count :: Int64, minLen :: Int, maxLen :: Int }
  | Doc        { count :: Int64, doc :: BsonDocument }
  | Array      { count :: Int64, values :: [SValue], minLen :: Int, maxLen :: Int }
  | Bin        { count :: Int64, minLen :: Int, maxLen :: Int }
  | Fun        { count :: Int64, minLen :: Int, maxLen :: Int }
  | Uuid       { count :: Int64 }
  | Md5        { count :: Int64 }
  | UserDef    { count :: Int64, minLen :: Int, maxLen :: Int }
  | ObjId      { count :: Int64 }
  | Bool       { count :: Int64, trues :: Int64, falses :: Int64 }
  | UTC        { count :: Int64, dmin :: Clock.UTCTime, dmax :: Clock.UTCTime }
  | Null       { count :: Int64 }
  | Regex      { count :: Int64 }
  | JavaScript { count :: Int64 }
  | Sym        { count :: Int64 }
  | Int32      { count :: Int64, min :: Int64, max :: Int64 }
  | Int64      { count :: Int64, min :: Int64, max :: Int64 }
  | Stamp      { count :: Int64 }
  | MinMax     { count :: Int64 }
  | MultiValue [SValue]
  | Unknown    { count :: Int64 }
  deriving (Show, Eq, Data.Data)

type BsonDocument = Map.Map Mdb.Label SValue

generateSchema :: Mdb.Database -> Mdb.Collection -> [Mdb.Document] -> BsonSchema
generateSchema db coll documents =
    let countAndSchema = map (\d -> (1, documentToSchema d)) documents
        (count, schema) = foldl' countAndMergeSchemas (0, Map.empty) countAndSchema
        schema' = mapToDocument schema
    in BsonSchema { database = db, collection = coll, schema = schema', count = count }
    where countAndMergeSchemas (gCount, gSchema) (count, schema) = (gCount + count, Map.unionWith mergeSValue gSchema schema)

documentToSchema :: Mdb.Document -> BsonDocument
documentToSchema = foldl' fieldToSchema Map.empty

fieldToSchema :: BsonDocument -> Mdb.Field -> BsonDocument
fieldToSchema map (label Mdb.:= value) = Map.insertWith mergeSValue label (bsonValueToSValue value) map


bsonValueToSValue :: Mdb.Value -> SValue
bsonValueToSValue (Mdb.Float val)                     = Float      { count = 1, fmin = val, fmax = val }
bsonValueToSValue (Mdb.String str)                    = String     { count = 1, minLen = Text.length str, maxLen = Text.length str }
bsonValueToSValue (Mdb.Bin (Mdb.Binary str))          = Bin        { count = 1, minLen = ByteStr.length str, maxLen = ByteStr.length str }
bsonValueToSValue (Mdb.Fun (Mdb.Function str))        = Fun        { count = 1, minLen = ByteStr.length str, maxLen = ByteStr.length str }
bsonValueToSValue (Mdb.Uuid _)                        = Uuid       { count = 1 }
bsonValueToSValue (Mdb.Md5 _)                         = Md5        { count = 1 }
bsonValueToSValue (Mdb.UserDef (Mdb.UserDefined str)) = UserDef    { count = 1, minLen = ByteStr.length str, maxLen = ByteStr.length str }
bsonValueToSValue (Mdb.ObjId _)                       = ObjId      { count = 1 }
bsonValueToSValue (Mdb.Bool bool)                     = Bool       { count = 1, trues = if bool then 1 else 0, falses = if bool then 0 else 1 }
bsonValueToSValue (Mdb.UTC time)                      = UTC        { count = 1, dmin = time, dmax = time }
bsonValueToSValue (Mdb.Null)                          = Null       { count = 1 }
bsonValueToSValue (Mdb.RegEx _)                       = Regex      { count = 1 }
bsonValueToSValue (Mdb.JavaScr _)                     = JavaScript { count = 1 }
bsonValueToSValue (Mdb.Sym _)                         = Sym        { count = 1 }
bsonValueToSValue (Mdb.Int32 int32)                   = Int32      { count = 1, min = fromIntegral int32, max = fromIntegral int32 }
bsonValueToSValue (Mdb.Int64 int64 )                  = Int64      { count = 1, min = int64, max = int64 }
bsonValueToSValue (Mdb.Stamp _)                       = Stamp      { count = 1 }
bsonValueToSValue (Mdb.MinMax _)                      = MinMax     { count = 1 }
bsonValueToSValue (Mdb.Doc document)                  = Doc        { count = 1, doc = documentToSchema document }
bsonValueToSValue (Mdb.Array array)                   = Array      { count = 1, values = array', minLen = length array, maxLen = length array }
    where array' = map (sumBy mergeSValue Unknown { count = -1 } . map bsonValueToSValue) $ groupBy equalByCtor array


mergeSValue :: SValue -> SValue -> SValue
mergeSValue (Float { count = lcount, fmin = lfmin, fmax =lfmax })
            (Float { count = rcount, fmin = rfmin, fmax = rfmax }) =
            Float { count = lcount + rcount, fmin = minOf lfmin rfmin, fmax = maxOf lfmax rfmax }
mergeSValue (String { count = lcount, minLen = lminLen, maxLen = lmaxLen })
            (String { count = rcount, minLen = rminLen, maxLen = rmaxLen }) =
            String { count = lcount + rcount, minLen = minOf lminLen rminLen, maxLen = maxOf lmaxLen rmaxLen }
mergeSValue (Doc { count = lcount, doc = ldoc })
            (Doc { count = rcount, doc = rdoc }) =
            Doc { count = lcount + rcount, doc = Map.unionWith mergeSValue ldoc rdoc }
mergeSValue (Array { count = lcount, values = lvalues, minLen = lminLen, maxLen = lmaxLen })
            (Array { count = rcount, values = rvalues, minLen = rminLen, maxLen = rmaxLen }) =
            Array { count = lcount + rcount, values = sumArray (lvalues ++ rvalues), minLen = minOf lminLen rminLen, maxLen = maxOf lmaxLen rmaxLen }
            where sumArray [] = []
                  sumArray x = [sumBy mergeSValue undefined x]
mergeSValue (Bin { count = lcount, minLen = lminLen, maxLen = lmaxLen })
            (Bin { count = rcount, minLen = rminLen, maxLen = rmaxLen }) =
            Bin { count = lcount + rcount, minLen = minOf lminLen rminLen, maxLen = maxOf lmaxLen rmaxLen }
mergeSValue (Fun { count = lcount, minLen = lminLen, maxLen = lmaxLen })
            (Fun { count = rcount, minLen = rminLen, maxLen = rmaxLen }) =
            Fun { count = lcount + rcount, minLen = minOf lminLen rminLen, maxLen = maxOf lmaxLen rmaxLen}
mergeSValue (Uuid { count = lcount })
            (Uuid { count = rcount }) =
            Uuid { count = lcount + rcount }
mergeSValue (Md5 { count = lcount })
            (Md5 { count = rcount }) =
            Md5 { count = lcount + rcount }
mergeSValue (UserDef { count = lcount, minLen = lminLen, maxLen = lmaxLen })
            (UserDef { count = rcount, minLen = rminLen, maxLen = rmaxLen }) =
            UserDef { count = lcount + rcount, minLen = minOf lminLen rminLen, maxLen = maxOf lmaxLen rmaxLen }
mergeSValue (ObjId { count = lcount })
            (ObjId { count = rcount }) =
            ObjId { count = lcount + rcount }
mergeSValue (Bool { count = lcount, trues = ltrues, falses = lfalses })
            (Bool { count = rcount, trues = rtrues, falses = rfalses }) =
            Bool { count = lcount + rcount, trues = ltrues + rtrues, falses = lfalses + rfalses }
mergeSValue (UTC { count = lcount, dmin = ldmin, dmax = ldmax })
            (UTC { count = rcount, dmin = rdmin, dmax = rdmax }) =
            UTC { count = lcount + rcount, dmin = minOf ldmin rdmin, dmax = maxOf ldmax rdmax }
mergeSValue (Null { count = lcount })
            (Null { count = rcount }) =
            Null { count = lcount + rcount }
mergeSValue (Regex { count = lcount })
            (Regex { count = rcount }) =
            Regex { count = lcount + rcount }
mergeSValue (JavaScript { count = lcount })
            (JavaScript { count = rcount }) =
            JavaScript { count = lcount + rcount }
mergeSValue (Sym { count = lcount })
            (Sym { count = rcount }) =
            Sym { count = lcount + rcount }
mergeSValue (Int32 { count = lcount, min = lmin, max = lmax })
            (Int32 { count = rcount, min = rmin, max = rmax }) =
            Int32 { count = lcount + rcount, min = minOf lmin rmin, max = maxOf lmax rmax }
mergeSValue (Int64 { count = lcount, min = lmin, max = lmax })
            (Int64 { count = rcount, min = rmin, max = rmax }) =
            Int64 { count = lcount + rcount, min = minOf lmin rmin, max = maxOf lmax rmax }
mergeSValue (Stamp { count = lcount })
            (Stamp { count = rcount }) =
            Stamp { count = lcount + rcount }
mergeSValue (MinMax { count = lcount })
            (MinMax { count = rcount }) =
            MinMax { count = lcount + rcount }
mergeSValue (Unknown { count = lcount })
            (Unknown { count = rcount }) =
            Unknown { count = lcount + rcount }
mergeSValue (MultiValue left)
            (MultiValue right) =
            MultiValue (left ++ right)
mergeSValue (MultiValue left)
            right = MultiValue (findOrAdd left right)
            where findOrAdd [] r = [r]
                  findOrAdd (x:xs) r
                      | x `ctorEq` r = mergeSValue x r : xs
                      | otherwise    = x : findOrAdd xs r
mergeSValue left
            right@(MultiValue _) =
            mergeSValue right left
mergeSValue left right = MultiValue [left, right]


instance Mdb.Val SValue where
    val (Float { count, fmin, fmax})            = Mdb.Doc [ strtype "Float", "Count" =: count, "Min" =: fmin, "Max" =: fmax ]
    val (String {count, minLen, maxLen})        = Mdb.Doc [ strtype "String", "Count" =: count, "MinLength" =: minLen, "MaxLength" =: maxLen ]
    val (Doc {count, doc})                      = Mdb.Doc [ "Type" =: mapToDocument doc, "Count" =: count, "Properties" =: length doc ]
    val (Array {count, values, minLen, maxLen}) = Mdb.Doc [ "Type" =: values, "Count" =: count, "MinLength" =: minLen, "MaxLength" =: maxLen ]
    val (Bin {count, minLen, maxLen})           = Mdb.Doc [ strtype "Bin", "Count" =: count, "MinLength" =: minLen, "MaxLength" =: maxLen ]
    val (Fun {count, minLen, maxLen})           = Mdb.Doc [ strtype "Fun", "Count" =: count, "MinLength" =: minLen, "MaxLength" =: maxLen ]
    val (Uuid {count})                          = Mdb.Doc [ strtype "Uuid", "Count" =: count ]
    val (Md5 {count})                           = Mdb.Doc [ strtype "Md5", "Count" =: count ]
    val (UserDef {count, minLen, maxLen})       = Mdb.Doc [ strtype "UserDef", "Count" =: count, "MinLength" =: minLen, "MaxLength" =: maxLen ]
    val (ObjId {count})                         = Mdb.Doc [ strtype "ObjectId", "Count" =: count ]
    val (Bool {count, trues, falses})           = Mdb.Doc [ strtype "Bool", "Count" =: count, "Trues" =: trues, "Falses" =: falses ]
    val (UTC {count, dmin, dmax})               = Mdb.Doc [ strtype "UTCTime", "Count" =: count, "Min" =: dmin, "Max" =: dmax ]
    val (Null {count})                          = Mdb.Doc [ strtype "Null", "Count" =: count ]
    val (Regex {count})                         = Mdb.Doc [ strtype "Regex", "Count" =: count ]
    val (JavaScript {count})                    = Mdb.Doc [ strtype "JavaScript", "Count" =: count ]
    val (Sym { count})                          = Mdb.Doc [ strtype "Symbol", "Count" =: count ]
    val (Int32 {count, min, max})               = Mdb.Doc [ strtype "Int32", "Count" =: count, "Min" =: min, "Max" =: max ]
    val (Int64 {count, min, max})               = Mdb.Doc [ strtype "Int64", "Count" =: count, "Min" =: min, "Max" =: max ]
    val (Stamp {count})                         = Mdb.Doc [ strtype "Stamp", "Count" =: count ]
    val (MinMax {count})                        = Mdb.Doc [ strtype "MinMax", "Count" =: count ]
    val (MultiValue values)                     = Mdb.Array $ map Mdb.val values
    val (Unknown {count})                       = Mdb.Doc [ strtype "Unknown", "Count" =: count ]

    cast' _ = Nothing

mapToDocument :: BsonDocument -> Mdb.Document
mapToDocument = map (\(k, a) -> k Mdb.=: a) . Map.toList

strtype :: String -> Mdb.Field
strtype str = "Type" Mdb.:= (Mdb.String $ Text.pack str)

equalByCtor :: Mdb.Value -> Mdb.Value -> Bool
equalByCtor (Mdb.Float _) (Mdb.Float _)     = True
equalByCtor (Mdb.String _) (Mdb.String _)   = True
equalByCtor (Mdb.Doc _) (Mdb.Doc _)         = True
equalByCtor (Mdb.Array _) (Mdb.Array _)     = True
equalByCtor (Mdb.Bin _) (Mdb.Bin _)         = True
equalByCtor (Mdb.Fun _) (Mdb.Fun _)         = True
equalByCtor (Mdb.Uuid _) (Mdb.Uuid _)       = True
equalByCtor (Mdb.Md5 _) (Mdb.Md5 _)         = True
equalByCtor (Mdb.UserDef _) (Mdb.UserDef _) = True
equalByCtor (Mdb.ObjId _) (Mdb.ObjId _)     = True
equalByCtor (Mdb.Bool _) (Mdb.Bool _)       = True
equalByCtor (Mdb.UTC _) (Mdb.UTC _)         = True
equalByCtor (Mdb.Null) (Mdb.Null)           = True
equalByCtor (Mdb.RegEx _) (Mdb.RegEx _)     = True
equalByCtor (Mdb.JavaScr _) (Mdb.JavaScr _) = True
equalByCtor (Mdb.Sym _) (Mdb.Sym _)         = True
equalByCtor (Mdb.Int32 _) (Mdb.Int32 _)     = True
equalByCtor (Mdb.Int64 _) (Mdb.Int64 _)     = True
equalByCtor (Mdb.Stamp _) (Mdb.Stamp _)     = True
equalByCtor (Mdb.MinMax _) (Mdb.MinMax _)   = True
equalByCtor _ _                             = False

minOf :: (Ord a) => a -> a -> a
minOf = Prelude.min

maxOf :: (Ord a) => a -> a -> a
maxOf = Prelude.max

ctorEq :: (Data.Data a) => a -> a -> Bool
ctorEq l r = Data.toConstr l == Data.toConstr r
