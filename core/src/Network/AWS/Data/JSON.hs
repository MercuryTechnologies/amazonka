{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.Data.JSON
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Data.JSON
    (
    -- * FromJSON
      FromJSON (..)
    , parseJSONText
    , eitherDecode
    , eitherDecode'

    -- ** Parser a
    , withObject
    , (.:)
    , (.:?)
    , (.!=)

    -- ** Either String a
    , eitherParseJSON
    , (.:>)
    , (.?>)

    -- * ToJSON
    , ToJSON   (..)
    , toJSONText
    , Value    (Object)
    , object
    , (.=)
    ) where

import           Data.Aeson            (eitherDecode, eitherDecode')
import qualified Data.Aeson.Key        as K
import qualified Data.Aeson.KeyMap     as KM
import           Data.Aeson.Types
import           Network.AWS.Data.Text

parseJSONText :: FromText a => String -> Value -> Parser a
parseJSONText n = withText n (either fail return . fromText)

toJSONText :: ToText a => a -> Value
toJSONText = String . toText

eitherParseJSON :: FromJSON a => Object -> Either String a
eitherParseJSON = parseEither parseJSON . Object

(.:>) :: FromJSON a => Object -> Text -> Either String a
(.:>) o k =
    case KM.lookup (K.fromText k) o of
        Nothing -> Left $ "key " ++ show k ++ " not present"
        Just v  -> parseEither parseJSON v

(.?>) :: FromJSON a => Object -> Text -> Either String (Maybe a)
(.?>) o k =
    case KM.lookup (K.fromText k) o of
        Nothing -> Right Nothing
        Just v  -> parseEither parseJSON v
