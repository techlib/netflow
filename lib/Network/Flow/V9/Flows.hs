{-|
Module      :  Network.Flow.V9.Flows
Description :  Flow Decoding
Copyright   :  (c) Jan Dvořák
License     :  MIT

Maintainer  :  mordae@mordae.eu
Stability   :  unstable
Portability :  non-portable (ghc)
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Flow.V9.Flows
( Flowset(..)
, Template(..)
, Type(..)
, Flow(..)
, decodeFlowset
, Templates
) where
  import BasePrelude hiding (union, empty)

  import Data.ByteString (ByteString)
  import Data.HashMap.Lazy (union, empty)
  import Data.Serialize.Get
  import Data.Aeson

  import qualified Data.HashMap.Strict as StrictHashMap

  import Network.Flow.V9.Fields


  type Templates = StrictHashMap.HashMap Word16 Template


  data Flowset
    = TemplateFlowset
      { flowsetTemplates  :: Templates }
    | DataFlowset
      { flowsetTemplateId :: !Word16
      , flowsetTime       :: !Word32
      , flowsetSource     :: !Word32
      , flowsetBodyBytes  :: ByteString }
    deriving (Show)


  data Template = Template
    { templateId       :: !Word16
    , templateScope    :: !Word
    , templateTypes    :: [Type] }
    deriving (Show)


  data Type = Type
    { typeNumber :: !Word16
    , typeSize   :: !Word16 }
    deriving (Show)


  data Flow = Flow
    { flowTime     :: !Word32
    , flowUptime   :: !Word32
    , flowSource   :: !Word32
    , flowTemplate :: !Word16
    , flowScope    :: Word
    , flowFields   :: Object }
    deriving (Show)


  decodeFlowset :: Word32 -> Flowset -> Templates -> (Maybe Flow, Templates)
  decodeFlowset _ (TemplateFlowset newTemplates) templates
    = (Nothing, StrictHashMap.union newTemplates templates)

  decodeFlowset uptime (DataFlowset tid time source body) templates
    = case findTemplate templates tid of
        -- Template not found means we have not received it yet.
        -- Since templates are sent out-of-band, we just keep going.
        Nothing
          -> (Nothing, templates)

        -- We can attempt to decode the body using this template.
        Just (Template _ scope fieldTypes)
          -> case decodeFields fieldTypes body of
              -- There is a small chance that the template is out of date,
              -- which can potentially lead to a decoding failure. Ignore.
              Nothing
                -> (Nothing, templates)

              -- Produce the (mostly) decoded flowset.
              Just fields
                -> (Just (Flow time uptime source tid scope fields), templates)


  findTemplate :: Templates -> Word16 -> Maybe Template
  findTemplate = flip StrictHashMap.lookup


  decodeFields :: [Type] -> ByteString -> Maybe Object
  decodeFields fieldTypes body
    = case runGet (getFields fieldTypes) body of
        Left _e -> Nothing
        Right r -> Just r


  getFields :: [Type] -> Get Object
  getFields [] = return empty

  getFields (fieldType:fieldTypes) = do
    field  <- getField fieldType
    fields <- getFields fieldTypes
    return $ union field fields


  getField :: Type -> Get Object
  getField (Type number size) = do
    bytes <- getBytes (fromIntegral size)
    return $ decodeField number bytes


  instance ToJSON Flow where
    toJSON f = object
      [ "time"     .= flowTime f
      , "uptime"   .= flowUptime f
      , "template" .= flowTemplate f
      , "scope"    .= flowScope f
      , "fields"   .= flowFields f ]


-- vim:set ft=haskell sw=2 ts=2 et:
