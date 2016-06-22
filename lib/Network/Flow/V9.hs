{-|
Module      :  Network.Flow.V9
Description :  NetFlow V9 Decoding
Copyright   :  (c) Jan Dvořák
License     :  MIT

Maintainer  :  mordae@mordae.eu
Stability   :  unstable
Portability :  non-portable (ghc)
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Network.Flow.V9
( Record(..)
, Flowset(..)
, Flow(..)
, DecodeException(..)
, decodeRecords
, decodeFlows
) where
  import BasePrelude hiding (yield, length)

  import Data.ByteString (ByteString)
  import Network.Socket (SockAddr)
  import Data.Serialize.Get
  import Pipes

  {-
  import Data.IP
  import Data.MAC
  -}

  import Network.Flow.V9.Decode


  data DecodeException = DecodeException String
    deriving (Show, Typeable)

  instance Exception DecodeException


  data Record = Record
    { recordVersion  :: !Word16
    , recordUptime   :: !Word32
    , recordTime     :: !Word32
    , recordSeqNo    :: !Word32
    , recordSource   :: !Word32
    , recordFlowsets :: [Flowset] }
    deriving (Show)


  {-
  data FlowInfo = FlowInfo
    { iInBytes    :: Maybe Word
    , iInPkts     :: Maybe Word
    , iFlows      :: Maybe Word
    , iProtocol   :: Maybe Word8
    , iTcpFlags   :: Maybe Word8
    , iSrcAddr    :: Maybe IP
    , iSrcPrefix  :: Maybe Word8
    , iSrcPort    :: Maybe Word16
    , iDstAddr    :: Maybe IP
    , iDstPrefix  :: Maybe Word16
    , iDstPort    :: Maybe Word16
    , iNextHop    :: Maybe IP
    , iMulPkts    :: Maybe Word
    , iMulBytes   :: Maybe Word
    , iOutBytes   :: Maybe Word
    , iOutPkts    :: Maybe Word
    , iMinPktLen  :: Maybe Word16
    , iMaxPktLen  :: Maybe Word16
    , iSrcMac     :: Maybe MAC
    , iDstMac     :: Maybe MAC
    , iSrcVlan    :: Maybe Word16
    , iDstVlan    :: Maybe Word16
    , iProtoVers  :: Maybe Word8
    , iDirection  :: Maybe Direction
    , iInDstMac   :: Maybe MAC
    , iOutSrcMac  :: Maybe MAC
    , iIfName     :: Maybe ByteString
    , iIfDesc     :: Maybe ByteString
    , iSampler    :: Maybe ByteString }
    deriving (Show)


  data Direction = Ingress | Egress
    deriving (Show, Eq, Enum)
  -}


  decodeRecords :: Pipe (ByteString, SockAddr) Record IO ()
  decodeRecords = forever $ do
    (dgram, _addr) <- await

    case runGet getRecord dgram of
      Left e   -> throw (DecodeException e)
      Right r  -> yield r


  getRecord :: Get Record
  getRecord = label "getRecord" $ do
    version  <- getWord16be
    count    <- getWord16be
    uptime   <- getWord32be
    time     <- getWord32be
    seqNo    <- getWord32be
    source   <- getWord32be
    flowsets <- getFlowsets time source count

    if version /= 9 then
      throw (DecodeException "expected NetFlow V9")
    else
      return $ Record version uptime time seqNo source flowsets


  getFlowsets :: Word32 -> Word32 -> Word16 -> Get [Flowset]
  getFlowsets _ _ 0 = return []
  getFlowsets time source n = label "getFlowsets" $ do
    x  <- getFlowset time source
    xs <- getFlowsets time source (n - 1)

    return (x:xs)


  getFlowset :: Word32 -> Word32 -> Get Flowset
  getFlowset time source = getTemplateFlowset
                           <|> getDataFlowset time source
                           <|> getOptionTemplateFlowset


  getTemplateFlowset :: Get Flowset
  getTemplateFlowset = label "getTemplateFlowset" $ do
    number' <- getWord16be

    if (number' > 0) then
      mzero
    else
      getNested getFlowsetLength $ do
        templates <- getTemplates
        return $ TemplateFlowset templates


  getTemplates :: Get [Template]
  getTemplates = label "getTemplates" $ do
      length' <- remaining

      if length' < 4 then
        return []
      else do
        number' <- getWord16be
        count   <- getWord16be
        types'  <- getTypes count
        more    <- getTemplates

        return $ Template number' 0 types' : more


  getTypes :: (Integral a) => a -> Get [Type]
  getTypes 0 = return []
  getTypes n = label "getTypes" $ do
    length' <- remaining

    if length' < 4 then
      return []
    else do
      number'   <- getWord16be
      size'     <- getWord16be
      moreTypes <- getTypes (n - 1)

      return $ Type number' size' : moreTypes


  getDataFlowset :: Word32 -> Word32 -> Get Flowset
  getDataFlowset time source = label "getDataFlowset" $ do
    number <- getWord16be

    if (number  < 256) then
      mzero
    else
      getNested getFlowsetLength $ do
        bodyLen <- remaining
        body    <- getBytes bodyLen

        return $ DataFlowset number time source body


  getOptionTemplateFlowset :: Get Flowset
  getOptionTemplateFlowset = label "getOptionTemplateFlowset" $ do
    number' <- getWord16be

    if (number' /= 1) then
      mzero
    else
      getNested getFlowsetLength $ do
        template' <- getWord16be
        scopeLen' <- getWord16be
        count     <- getWord16be
        types'    <- getTypes (count `shiftR` 2)

        remaining >>= skip
        return $ TemplateFlowset [Template template' scopeLen' types']


  getFlowsetLength :: Get Int
  getFlowsetLength = label "getFlowsetLength" $ do
    length' <- getWord16be
    return $ fromIntegral length' - 4


  decodeFlows :: Pipe Record Flow IO ()
  decodeFlows = decodeFlows' []


  decodeFlows' :: [Template] -> Pipe Record Flow IO ()
  decodeFlows' templates = do
    -- Obtain new record to parse.
    (Record _ _ _ _ _ flowsets) <- await

    -- Decode all the flowsets, collecting both flows and templates.
    -- All flows that came after the templates used those templates.
    let (flows, templates') = decodeFlowsets flowsets templates

    -- Yield all the flows.
    for_ flows yield

    -- Continue with the new set of templates.
    decodeFlows' templates'


  decodeFlowsets :: [Flowset] -> [Template] -> ([Flow], [Template])
  decodeFlowsets [] templates = ([], templates)

  decodeFlowsets (flowset:flowsets) templates
    = case decodeFlowset flowset templates of
        (Nothing, templates')
          -> let (more, templates'') = decodeFlowsets flowsets templates'
              in (more, templates'')

        (Just flow, templates')
          -> let (more, templates'') = decodeFlowsets flowsets templates'
              in (flow:more, templates'')


-- vim:set ft=haskell sw=2 ts=2 et:
