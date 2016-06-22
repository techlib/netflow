{-|
Module      :  Network.Flow.V9.Decode
Description :  Stateful Template-Based Decoding
Copyright   :  (c) Jan Dvořák
License     :  MIT

Maintainer  :  mordae@mordae.eu
Stability   :  unstable
Portability :  non-portable (ghc)
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Flow.V9.Decode
( Flowset(..)
, Template(..)
, Type(..)
, Flow(..)
, Field(..)
, decodeFlowset
) where
  import BasePrelude
  import Data.ByteString (ByteString, unpack)
  import Data.Serialize.Get


  data Flowset
    = TemplateFlowset
      { flowsetTemplates  :: [Template] }
    | DataFlowset
      { flowsetTemplateId :: !Word16
      , flowsetTime       :: !Word32
      , flowsetSource     :: !Word32
      , flowsetBodyBytes  :: ByteString }
    deriving (Show)


  data Template = Template
    { templateId       :: !Word16
    , templateScopeLen :: !Word16
    , templateTypes    :: [Type] }
    deriving (Show)


  data Type = Type
    { typeNumber :: !Word16
    , typeSize   :: !Word16 }
    deriving (Show)


  data Flow = Flow
    { flowTime     :: !Word32
    , flowSource   :: !Word32
    , flowTemplate :: !Word16
    , flowScope    :: Word
    , flowFields   :: [Field] }
    deriving (Show)


  data Field = Field
    { fieldNumber :: !Word16
    , fieldValue  :: Word
    , fieldBytes  :: ByteString }
    deriving (Show)


  decodeFlowset :: Flowset -> [Template] -> (Maybe Flow, [Template])
  decodeFlowset (TemplateFlowset newTemplates) templates
    = (Nothing, nubBy sameTemplateId (newTemplates <> templates))

  decodeFlowset (DataFlowset tid time source body) templates
    = case findTemplate templates tid of
        -- Template not found means we have not received it yet.
        -- Since templates are sent out-of-band, we just keep going.
        Nothing
          -> (Nothing, templates)

        -- We can attempt to decode the body using this template.
        Just (Template _ scopeLen fieldTypes)
          -> case decodeFields scopeLen fieldTypes body of
              -- There is a small chance that the template is out of date,
              -- which can potentially lead to a decoding failure. Ignore.
              Nothing
                -> (Nothing, templates)

              -- Produce the (mostly) decoded flowset.
              Just (scope, fields)
                -> (Just (Flow time source tid scope fields), templates)


  sameTemplateId :: Template -> Template -> Bool
  sameTemplateId x y = xno == yno
    where xno = templateId (x :: Template)
          yno = templateId (y :: Template)


  findTemplate :: [Template] -> Word16 -> Maybe Template
  findTemplate templates templateId' = find matching templates
      where matching t = (templateId' == templateId t)


  decodeFields :: Word16 -> [Type] -> ByteString -> Maybe (Word, [Field])
  decodeFields scopeLen fieldTypes body
    = case runGet (getBody scopeLen fieldTypes) body of
        Left _  -> Nothing
        Right r -> Just r


  getBody :: Word16 -> [Type] -> Get (Word, [Field])
  getBody scopeLen fieldTypes = do
    scope  <- getBytes (fromIntegral scopeLen)
    fields <- getFields fieldTypes
    return (roll scope, fields)


  getFields :: [Type] -> Get [Field]
  getFields [] = return []

  getFields (fieldType:fieldTypes) = do
    field  <- getField fieldType
    fields <- getFields fieldTypes
    return (field:fields)


  getField :: Type -> Get Field
  getField (Type number size) = do
    bytes <- getBytes (fromIntegral size)
    return (Field number (roll bytes) bytes)


  roll :: ByteString -> Word
  roll = foldl' (\c n -> c `shiftL` 8 + n) 0 . map fromIntegral . unpack


-- vim:set ft=haskell sw=2 ts=2 et:
