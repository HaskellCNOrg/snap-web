{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-
FROM: https://gist.github.com/2725383
-}

module Data.Baeson.Types
       ( Parser
       , Result(..)
       , ToBSON(..)
       , FromBSON(..)
       , ToBSONDoc(..)
       , FromBSONDoc(..)
       , BSONKey (..)
       , parse
       , parseEither
       , parseMaybe
       , typeMismatch
       , (.:)
       , (.:?)
       , (.=)
       , (.!=)
       , document
       , mkDotName
       , BSON.Value(..)
       )
       where

import Control.Applicative
import Control.Monad
import Data.Monoid (Monoid(..))
import Data.Typeable

import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import qualified Data.CompactString.UTF8 as CS

import qualified Data.Bson as BSON(Value(..), Document, look, Field(..),
                                   Binary(..))

data Result a = Error String
              | Success a
                deriving (Eq, Show, Typeable)

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap _ (Error err) = Error err
    {-# INLINE fmap #-}

instance Monad Result where
    return = Success
    {-# INLINE return #-}
    Success a >>= k = k a
    Error err >>= _ = Error err
    {-# INLINE (>>=) #-}

instance Applicative Result where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = ap
    {-# INLINE (<*>) #-}

instance MonadPlus Result where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a@(Success _) _ = a
    mplus _ b             = b
    {-# INLINE mplus #-}

instance Alternative Result where
    empty = mzero
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance Monoid (Result a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}


-- | Failure continuation.
type Failure f r   = String -> f r
-- | Success continuation.
type Success a f r = a -> f r

-- | A continuation-based parser type.
newtype Parser a = Parser {
      runParser :: forall f r.
                   Failure f r
                -> Success a f r
                -> f r
    }

instance Monad Parser where
    m >>= g = Parser $ \kf ks -> let ks' a = runParser (g a) kf ks
                                 in runParser m kf ks'
    {-# INLINE (>>=) #-}
    return a = Parser $ \_kf ks -> ks a
    {-# INLINE return #-}
    fail msg = Parser $ \kf _ks -> kf msg
    {-# INLINE fail #-}

instance Functor Parser where
    fmap f m = Parser $ \kf ks -> let ks' a = ks (f a)
                                  in runParser m kf ks'
    {-# INLINE fmap #-}

instance Applicative Parser where
    pure  = return
    {-# INLINE pure #-}
    (<*>) = apP
    {-# INLINE (<*>) #-}

instance Alternative Parser where
    empty = fail "empty"
    {-# INLINE empty #-}
    (<|>) = mplus
    {-# INLINE (<|>) #-}

instance MonadPlus Parser where
    mzero = fail "mzero"
    {-# INLINE mzero #-}
    mplus a b = Parser $ \kf ks -> let kf' _ = runParser b kf ks
                                   in runParser a kf' ks
    {-# INLINE mplus #-}

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    {-# INLINE mempty #-}
    mappend = mplus
    {-# INLINE mappend #-}

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}


-- | Run a 'Parser'.
parse :: (a -> Parser b) -> a -> Result b
parse m v = runParser (m v) Error Success
{-# INLINE parse #-}

-- | Run a 'Parser' with a 'Maybe' result type.
parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe m v = runParser (m v) (const Nothing) Just
{-# INLINE parseMaybe #-}

-- | Run a 'Parser' with an 'Either' result type.
parseEither :: (a -> Parser b) -> a -> Either String b
parseEither m v = runParser (m v) Left Right


class ToBSON a where
  toBSON :: a -> BSON.Value

class ToBSONDoc a where
  toBSONDoc :: a -> BSON.Document

class (Ord a) => BSONKey a where
  toBSONKey :: a -> CS.CompactString
  fromBSONKey :: CS.CompactString -> a

instance BSONKey T.Text where
  toBSONKey = textToCompactString
  fromBSONKey = compactStringToText

instance ToBSON a => ToBSONDoc (Map T.Text a) where
  toBSONDoc = Map.foldrWithKey (\k v -> (:)(k.=v)) []

instance (ToBSON a, BSONKey k) => ToBSONDoc (Map k a) where
  toBSONDoc = Map.foldrWithKey (\k v -> (:)(toBSONKey k BSON.:= toBSON v)) []


instance ToBSON Bool where
  toBSON = BSON.Bool
  {-# INLINE toBSON #-}

instance ToBSON Double where
  toBSON = BSON.Float
  {-# INLINE toBSON #-}

instance ToBSON Float where
  toBSON = BSON.Float . realToFrac
  {-# INLINE toBSON #-}

instance ToBSON Int where
  toBSON = BSON.Int64 . fromIntegral

instance ToBSON UTCTime where
  toBSON = BSON.UTC


instance ToBSON String where
  toBSON = BSON.String . CS.pack
  {-# INLINE toBSON #-}

instance ToBSON [BSON.Value] where
  toBSON = BSON.Array
  {-# INLINE toBSON #-}

instance (ToBSON a) => ToBSON [a] where
  toBSON = BSON.Array . map toBSON
  {-# INLINE toBSON #-}

instance ToBSON T.Text where
  toBSON = BSON.String . textToCompactString
  {-# INLINE toBSON #-}

instance ToBSON LT.Text where
  toBSON = BSON.String . lazyTextToCompactString
  {-# INLINE toBSON #-}

instance ToBSON BS.ByteString where
  toBSON = BSON.Bin . BSON.Binary
  {-# INLINE toBSON #-}

instance ToBSON BSON.Document where
  toBSON = BSON.Doc
  {-# INLINE toBSON #-}

instance ToBSON a => ToBSON (Maybe a) where
  toBSON Nothing = BSON.Null
  toBSON (Just x) = toBSON x
  {-# INLINE toBSON #-}

instance ToBSON a => ToBSON (Set a) where
  toBSON = toBSON . Set.toList

instance (ToBSON a, ToBSON b) => ToBSON (a,b) where
  toBSON (a,b) = BSON.Array [toBSON a, toBSON b]
  {-# INLINE toBSON #-}

instance (ToBSON a, ToBSON b, ToBSON c) => ToBSON (a,b,c) where
  toBSON (a,b,c) = BSON.Array [toBSON a, toBSON b, toBSON c]
  {-# INLINE toBSON #-}

instance ToBSONDoc a => ToBSON a where
  toBSON = BSON.Doc . toBSONDoc
  {-# INLINE toBSON #-}

class FromBSONDoc a where
  fromBSONDoc :: BSON.Document->Parser a

class FromBSON a where
  fromBSON :: BSON.Value -> Parser a

instance FromBSON Bool where
  fromBSON (BSON.Bool b) = pure b
  fromBSON b = typeMismatch "Bool" b
  {-# INLINE fromBSON #-}

instance FromBSON Double where
  fromBSON (BSON.Float x) = pure x
  fromBSON (BSON.Int32 x) = pure $! fromIntegral x
  fromBSON (BSON.Int64 x) = pure $! fromIntegral x
  fromBSON b = typeMismatch "Float, Int32, Int64" b
  {-# INLINE fromBSON #-}

instance FromBSON Float where
  fromBSON (BSON.Float x) = pure $! realToFrac x
  fromBSON (BSON.Int32 x) = pure $! fromIntegral x
  fromBSON (BSON.Int64 x) = pure $! fromIntegral x
  fromBSON b = typeMismatch "Int32, Int64 or Float" b
  {-# INLINE fromBSON #-}

instance FromBSON Int where
  fromBSON (BSON.Int32 x) = pure $! fromIntegral x
  fromBSON (BSON.Int64 x) = pure $! fromEnum x
  fromBSON (BSON.Float x) = pure $! round x
  fromBSON b = typeMismatch "Int32, Int64 or Float" b
  {-# INLINE fromBSON #-}

instance FromBSON UTCTime where
  fromBSON (BSON.UTC x) = pure x
  fromBSON b = typeMismatch "UTC" b

instance FromBSON String where
  fromBSON (BSON.String x )= pure $! CS.unpack x
  fromBSON b = typeMismatch "String" b
  {-# INLINE fromBSON #-}

instance FromBSON T.Text where
  fromBSON (BSON.String x) = pure $! compactStringToText x
  fromBSON b = typeMismatch "String" b
  {-# INLINE fromBSON #-}

instance FromBSON LT.Text where
  fromBSON (BSON.String x) = pure $! compactStringToLazyText x
  fromBSON b = typeMismatch "String" b
  {-# INLINE fromBSON #-}

instance FromBSON BS.ByteString where
  fromBSON (BSON.Bin (BSON.Binary x)) = pure x
  fromBSON b = typeMismatch "Binary" b
  {-# INLINE fromBSON #-}


instance (FromBSON a) => FromBSON [a] where
  fromBSON (BSON.Array x) = mapM fromBSON x
  fromBSON b = typeMismatch "Array" b
  {-# INLINE fromBSON #-}

instance FromBSON a => FromBSON (Maybe a) where
  fromBSON BSON.Null = pure Nothing
  fromBSON v = Just <$> fromBSON v
  {-# INLINE fromBSON #-}

instance (FromBSON a, FromBSON b) => FromBSON (a,b) where
  fromBSON (BSON.Array (a:b:[])) = (,) <$> fromBSON a <*> fromBSON b
  fromBSON (BSON.Array b) = fail $ "Expected array of size 2"
  fromBSON b = typeMismatch "Array" b
  {-# INLINE fromBSON #-}

instance (FromBSON a, FromBSON b, FromBSON c) => FromBSON (a,b,c) where
  fromBSON (BSON.Array (a:b:c:[])) = (,,) <$> fromBSON a <*>
                                   fromBSON b <*> fromBSON c
  fromBSON (BSON.Array b) = fail $ "Expected array of size 3"
  fromBSON b = typeMismatch "Array" b
  {-# INLINE fromBSON #-}

instance (FromBSON a, Ord a) => FromBSON (Set a) where
  fromBSON a = Set.fromList <$> fromBSON a
  {-# INLINE fromBSON #-}


instance FromBSON BSON.Document where
  fromBSON (BSON.Doc x) = pure x
  fromBSON b = typeMismatch "Document" b
  {-# INLINE fromBSON #-}

instance FromBSONDoc a => FromBSON a where
  fromBSON (BSON.Doc x) = fromBSONDoc x
  fromBSON b = typeMismatch "Document" b
  {-# INLINE fromBSON #-}

instance FromBSON a => FromBSONDoc (Map T.Text a) where
  fromBSONDoc d = Map.fromList <$> mapM go d
    where go (k BSON.:= v) = (,) <$> pure (compactStringToText k) <*> fromBSON v

instance (FromBSON a, BSONKey k) => FromBSONDoc (Map k a) where
  fromBSONDoc d = Map.fromList <$> mapM go d
    where go (k BSON.:= v) = (,) <$> pure (fromBSONKey k) <*> fromBSON v

compactStringToText :: CS.CompactString -> T.Text
compactStringToText = T.decodeUtf8 . CS.toByteString

textToCompactString :: T.Text -> CS.CompactString
textToCompactString = CS.fromByteString_ . T.encodeUtf8

compactStringToLazyText :: CS.CompactString -> LT.Text
compactStringToLazyText =
  LT.decodeUtf8 . (\x -> BSL.fromChunks [x]) . CS.toByteString

lazyTextToCompactString :: LT.Text -> CS.CompactString
lazyTextToCompactString =
  CS.fromByteString_ . BS.concat . BSL.toChunks . LT.encodeUtf8

typeMismatch :: String -> BSON.Value -> Parser a
typeMismatch what was =
  fail $ "Expected '" ++ what ++ "' found '"++ describeType was ++"'"
  where
    describeType :: BSON.Value -> String
    describeType (BSON.Float   _) = "Float"
    describeType (BSON.String  _) = "String"
    describeType (BSON.Doc     _) = "Document"
    describeType (BSON.Array   _) = "Array"
    describeType (BSON.Bin     _) = "Binary"
    describeType (BSON.Fun     _) = "Function"
    describeType (BSON.Uuid    _) = "UUID"
    describeType (BSON.Md5     _) = "MD5"
    describeType (BSON.UserDef _) = "UserDefined"
    describeType (BSON.ObjId   _) = "ObjectId"
    describeType (BSON.Bool    _) = "Bool"
    describeType (BSON.UTC     _) = "UTCTime"
    describeType (BSON.Null     ) = "null"
    describeType (BSON.RegEx   _) = "RegEx"
    describeType (BSON.JavaScr _) = "JavaScript"
    describeType (BSON.Sym     _) = "Symbol"
    describeType (BSON.Int32   _) = "Int32"
    describeType (BSON.Int64   _) = "Int64"
    describeType (BSON.Stamp   _) = "Stamp"
    describeType (BSON.MinMax  _) = "MinMax"


(.:) :: (FromBSON a) => BSON.Document -> T.Text -> Parser a
obj .: key = case BSON.look (textToCompactString key) obj of
               Nothing -> fail $ "key " ++ show key ++ " not present"
               Just v  -> fromBSON v
{-# INLINE (.:) #-}

-- | Retrieve the value associated with the given key of an 'Object'.
-- The result is 'Nothing' if the key is not present, or 'empty' if
-- the value cannot be converted to the desired type.
--
-- This accessor is most useful if the key and value can be absent
-- from an object without affecting its validity.  If the key and
-- value are mandatory, use '(.:)' instead.
(.:?) :: (FromBSON a) => BSON.Document -> T.Text -> Parser (Maybe a)
obj .:? key = case BSON.look (textToCompactString key) obj of
               Nothing -> pure Nothing
               Just v  -> fromBSON v

-- | Construct a 'Pair' from a key and a value.
(.=) :: ToBSON a => T.Text -> a -> BSON.Field
name .= value = textToCompactString name BSON.:= toBSON value
{-# INLINE (.=) #-}

(.!=) :: Parser (Maybe a) -> a -> Parser a
pmval .!= val = fromMaybe val <$> pmval
{-# INLINE (.!=) #-}


document :: [BSON.Field] -> BSON.Value
document = BSON.Doc
{-# INLINE document #-}

mkDotName :: [T.Text] -> CS.CompactString
mkDotName = textToCompactString . T.intercalate "."