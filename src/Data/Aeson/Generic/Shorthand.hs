{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-|
Module      : Data.Aeson.Generic.Shorthand
Description : A convenient shorthand for defining ToJSON and FromJSON instances.
-}
module Data.Aeson.Generic.Shorthand
  ( GenericToFromJSON(..)
  , snakeOptions
  , snakeOptionsMod
  , SnakeFields
  , camelOptions
  , camelOptionsMod
  , nullaryOptions
  , CamelFields
  , TagSingleConstructors
  , UnwrapUnaryRecords
  , NotAllNullaryToStringTag
  , OmitNothingFields
  , SumEncodingUntaggedValue
  , SumEncodingObjectWithSingleField
  , SumEncodingTwoElemArray
  ) where

import Data.Aeson    (FromJSON (..), GFromJSON, GToEncoding, GToJSON,
                      Options (..), SumEncoding (..), ToJSON (..), Zero,
                      defaultOptions, genericParseJSON, genericToEncoding,
                      genericToJSON)
import Data.Char     (isUpper)
import Data.Function ((&))
import Data.Kind     (Type)
import Data.Proxy    (Proxy (..))
import GHC.Generics  (Generic, Rep)
import Text.Casing   (camel, quietSnake)

class JSONOptions (opts :: [Type]) where
  jsonOptions :: Proxy opts -> Options

instance JSONOptions '[] where
  jsonOptions _ = defaultOptions

newtype GenericToFromJSON (opts :: [Type]) a = GenericToFromJSON { unGenericToFromJSON :: a }
  deriving (Generic)

instance (Generic a, GToJSON Zero (Rep a), GToEncoding Zero (Rep a), JSONOptions opts) => ToJSON (GenericToFromJSON opts a) where
  toJSON     = genericToJSON (jsonOptions (Proxy :: Proxy opts)) . unGenericToFromJSON
  toEncoding = genericToEncoding (jsonOptions (Proxy :: Proxy opts)) . unGenericToFromJSON

instance (Generic a, GFromJSON Zero (Rep a), JSONOptions opts) => FromJSON (GenericToFromJSON opts a) where
  parseJSON = fmap GenericToFromJSON . genericParseJSON (jsonOptions (Proxy :: Proxy opts))

data SnakeFields

snakeOptionsMod :: Options -> Options
snakeOptionsMod opts = opts
  { fieldLabelModifier = quietSnake . dropWhile (not . isUpper)
  -- ^ Convert our field names into JSON-style field names by dropping everything
  -- up to the first upper case letter, then snake-casing.
  -- E.g. _organizationUuid -> uuid
  --      _metaConceptType  -> concept_type
  }

snakeOptions :: Options
snakeOptions = defaultOptions & snakeOptionsMod

instance JSONOptions opts => JSONOptions (SnakeFields ': opts) where
  jsonOptions _ = jsonOptions (Proxy :: Proxy opts) & snakeOptionsMod

data CamelFields

camelOptionsMod :: Options -> Options
camelOptionsMod opts = opts
  { fieldLabelModifier = camel . dropWhile (not . isUpper)
  -- ^ Convert our field names by dropping everything
  -- up to the first upper case letter, then camel-casing.
  -- E.g. _organizationUuid -> uuid
  --      _metaConceptType  -> conceptType
  }

camelOptions :: Options
camelOptions = defaultOptions & camelOptionsMod

nullaryOptions :: Options
nullaryOptions = defaultOptions
  { sumEncoding = UntaggedValue }

instance JSONOptions opts => JSONOptions (CamelFields ': opts) where
  jsonOptions _ = jsonOptions (Proxy :: Proxy opts) & camelOptionsMod

data TagSingleConstructors
instance JSONOptions opts => JSONOptions (TagSingleConstructors ': opts) where
  jsonOptions _ = (jsonOptions (Proxy :: Proxy opts)) { tagSingleConstructors = True }

data UnwrapUnaryRecords
instance JSONOptions opts => JSONOptions (UnwrapUnaryRecords ': opts) where
  jsonOptions _ = (jsonOptions (Proxy :: Proxy opts)) { unwrapUnaryRecords = True }

data NotAllNullaryToStringTag
instance JSONOptions opts => JSONOptions (NotAllNullaryToStringTag ': opts) where
  jsonOptions _ = (jsonOptions (Proxy :: Proxy opts)) { allNullaryToStringTag = False }

data OmitNothingFields
instance JSONOptions opts => JSONOptions (OmitNothingFields ': opts) where
  jsonOptions _ = (jsonOptions (Proxy :: Proxy opts)) { omitNothingFields = True }

data SumEncodingUntaggedValue
instance JSONOptions opts => JSONOptions (SumEncodingUntaggedValue ': opts) where
  jsonOptions _ = (jsonOptions (Proxy :: Proxy opts)) { sumEncoding = UntaggedValue }

data SumEncodingObjectWithSingleField
instance JSONOptions opts => JSONOptions (SumEncodingObjectWithSingleField ': opts) where
  jsonOptions _ = (jsonOptions (Proxy :: Proxy opts)) { sumEncoding = ObjectWithSingleField }

data SumEncodingTwoElemArray
instance JSONOptions opts => JSONOptions (SumEncodingTwoElemArray ': opts) where
  jsonOptions _ = (jsonOptions (Proxy :: Proxy opts)) { sumEncoding = TwoElemArray }

