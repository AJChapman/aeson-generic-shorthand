# Aeson Generic Shorthand

This simple library provides a convenient way of defining ToJSON and FromJSON instances for data types, with options allowing you to customise some of the behaviour, in a single `deriving` line per data type.
It is based on a talk given at [CanFP](https://www.meetup.com/CanFPG/) by Alex Mason.

## Usage Example

```
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}

module Test where

import Data.Aeson                   (FromJSON, ToJSON)
import Data.Aeson.Generic.Shorthand (GenericToFromJSON (..), OmitNothingFields,
                                     SnakeFields)
import GHC.Generics                 (Generic)

data Address = Address
  { _addrStreetNumber :: Int
  , _addrStreetName   :: String
  , _addrSuburb       :: Maybe String
  } deriving (Eq, Ord, Show, Generic)
  deriving (ToJSON, FromJSON) via (GenericToFromJSON '[SnakeFields, OmitNothingFields] Address)
```

```
λ :m +Test
λ :m +Data.Aeson
λ encode (Address 1 "Bork St." Nothing)
"{\"street_number\":1,\"street_name\":\"Bork St.\"}"
λ encode (Address 1 "Bork St." (Just "Somewhere"))
"{\"street_number\":1,\"street_name\":\"Bork St.\",\"suburb\":\"Somewhere\"}"

```
