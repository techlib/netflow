{-|
Module      :  Network.Flow.V9.Fields
Description :  Field Decoding
Copyright   :  (c) Jan Dvořák
License     :  MIT

Maintainer  :  mordae@mordae.eu
Stability   :  unstable
Portability :  non-portable (ghc)
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.Flow.V9.Fields (decodeField)
where
  import BasePrelude hiding (empty, union, lookup)

  import Data.MAC (MAC, toMAC)
  import Data.IP (IPv4, IPv6, toIPv4, toIPv6b)
  import Data.Text.Encoding (decodeUtf8, decodeUtf8')
  import Data.Serialize.Get
  import Data.Serialize.IEEE754

  import Data.HashMap.Lazy (HashMap, singleton, fromList, lookup, empty)
  import Data.ByteString (ByteString)
  import Data.Text (Text)
  import Data.Aeson

  import qualified Data.ByteString.Base64 as Base64
  import qualified Data.ByteString as BS


  decodeField :: Word16 -> ByteString -> Object
  decodeField fid bs = case lookup fid decoders of
                           Just dec -> dec bs
                           Nothing  -> decodeOther fid bs


  decodeField' :: (DecodeAs a) => Text -> (a -> Value) -> ByteString -> Object
  decodeField' name jsonify bs = case decodeAs bs of
                                   Just val -> singleton name $ jsonify val
                                   Nothing  -> empty


  decodeOther :: Word16 -> ByteString -> Object
  decodeOther fid bs = singleton name value
    where name = fromString $ show fid
          value = toJSON $ decodeUtf8 $ Base64.encode bs


  class DecodeAs a where
    decodeAs :: ByteString -> Maybe a


  instance DecodeAs Bytes where
    decodeAs = Just . Bytes


  instance DecodeAs Bool where
    decodeAs bs = case (roll bs :: Word) of
                    1 -> Just True
                    2 -> Just False
                    _ -> Nothing


  instance DecodeAs Word where
    decodeAs = Just . roll


  instance DecodeAs Word8 where
    decodeAs = Just . roll


  instance DecodeAs Word16 where
    decodeAs = Just . roll


  instance DecodeAs Word32 where
    decodeAs = Just . roll


  instance DecodeAs Word64 where
    decodeAs = Just . roll


  instance DecodeAs Int64 where
    decodeAs bs =
      case runGet getInt64be bs of
        Left _e -> Nothing
        Right v -> Just v


  instance DecodeAs Text where
    decodeAs bs =
      case decodeUtf8' bs of
        Left _e -> Nothing
        Right v -> Just v


  instance DecodeAs Double where
    decodeAs bs =
      case runGet getFloat64be bs of
        Left _e -> Nothing
        Right v -> Just v


  instance DecodeAs Addr4 where
    decodeAs bs =
      case BS.length bs of
        4 -> Just $ Addr4 $ toIPv4 $ map fromIntegral $ BS.unpack bs
        _ -> Nothing


  instance DecodeAs Addr6 where
    decodeAs bs =
      case BS.length bs of
        16 -> Just $ Addr6 $ toIPv6b $ map fromIntegral $ BS.unpack bs
        _x -> Nothing


  instance DecodeAs MAC where
    decodeAs bs =
      case BS.length bs of
        6 -> Just $ toMAC $ BS.unpack bs
        _ -> Nothing


  newtype Bytes = Bytes ByteString

  instance ToJSON Bytes where
    toJSON (Bytes bs) = toJSON $ decodeUtf8 $ Base64.encode bs


  newtype Addr6 = Addr6 IPv6

  instance ToJSON Addr6 where
    toJSON (Addr6 ipv6) = toJSON $ show ipv6


  newtype Addr4 = Addr4 IPv4

  instance ToJSON Addr4 where
    toJSON (Addr4 ipv4) = toJSON $ show ipv4


  roll :: (Integral a, Bits a) => ByteString -> a
  roll = foldl' (\c n -> c `shiftL` 8 + n) 0 . map fromIntegral . BS.unpack


  decoders :: HashMap Word16 (ByteString -> Object)
  decoders =
    fromList
      [ (1, decodeField' "OctetDeltaCount" (toJSON @Word64))
      , (2, decodeField' "PacketDeltaCount" (toJSON @Word64))
      , (3, decodeField' "DeltaFlowCount" (toJSON @Word64))
      , (4, decodeField' "ProtocolIdentifier" (toJSON @Word8))
      , (5, decodeField' "IpClassOfService" (toJSON @Word8))
      , (6, decodeField' "TcpControlBits" (toJSON @Word16))
      , (7, decodeField' "SourceTransportPort" (toJSON @Word16))
      , (8, decodeField' "SourceIPv4Address" (toJSON @Addr4))
      , (9, decodeField' "SourceIPv4PrefixLength" (toJSON @Word8))
      , (10, decodeField' "IngressInterface" (toJSON @Word32))
      , (11, decodeField' "DestinationTransportPort" (toJSON @Word16))
      , (12, decodeField' "DestinationIPv4Address" (toJSON @Addr4))
      , (13, decodeField' "DestinationIPv4PrefixLength" (toJSON @Word8))
      , (14, decodeField' "EgressInterface" (toJSON @Word32))
      , (15, decodeField' "IpNextHopIPv4Address" (toJSON @Addr4))
      , (16, decodeField' "BgpSourceAsNumber" (toJSON @Word32))
      , (17, decodeField' "BgpDestinationAsNumber" (toJSON @Word32))
      , (18, decodeField' "BgpNextHopIPv4Address" (toJSON @Addr4))
      , (19, decodeField' "PostMCastPacketDeltaCount" (toJSON @Word64))
      , (20, decodeField' "PostMCastOctetDeltaCount" (toJSON @Word64))
      , (21, decodeField' "FlowEndSysUpTime" (toJSON @Word32))
      , (22, decodeField' "FlowStartSysUpTime" (toJSON @Word32))
      , (23, decodeField' "PostOctetDeltaCount" (toJSON @Word64))
      , (24, decodeField' "PostPacketDeltaCount" (toJSON @Word64))
      , (25, decodeField' "MinimumIpTotalLength" (toJSON @Word64))
      , (26, decodeField' "MaximumIpTotalLength" (toJSON @Word64))
      , (27, decodeField' "SourceIPv6Address" (toJSON @Addr6))
      , (28, decodeField' "DestinationIPv6Address" (toJSON @Addr6))
      , (29, decodeField' "SourceIPv6PrefixLength" (toJSON @Word8))
      , (30, decodeField' "DestinationIPv6PrefixLength" (toJSON @Word8))
      , (31, decodeField' "FlowLabelIPv6" (toJSON @Word32))
      , (32, decodeField' "IcmpTypeCodeIPv4" (toJSON @Word16))
      , (33, decodeField' "IgmpType" (toJSON @Word8))
      , (34, decodeField' "SamplingInterval" (toJSON @Word32))
      , (35, decodeField' "SamplingAlgorithm" (toJSON @Word8))
      , (36, decodeField' "FlowActiveTimeout" (toJSON @Word16))
      , (37, decodeField' "FlowIdleTimeout" (toJSON @Word16))
      , (38, decodeField' "EngineType" (toJSON @Word8))
      , (39, decodeField' "EngineId" (toJSON @Word8))
      , (40, decodeField' "ExportedOctetTotalCount" (toJSON @Word64))
      , (41, decodeField' "ExportedMessageTotalCount" (toJSON @Word64))
      , (42, decodeField' "ExportedFlowRecordTotalCount" (toJSON @Word64))
      , (43, decodeField' "Ipv4RouterSc" (toJSON @Addr4))
      , (44, decodeField' "SourceIPv4Prefix" (toJSON @Addr4))
      , (45, decodeField' "DestinationIPv4Prefix" (toJSON @Addr4))
      , (46, decodeField' "MplsTopLabelType" (toJSON @Word8))
      , (47, decodeField' "MplsTopLabelIPv4Address" (toJSON @Addr4))
      , (48, decodeField' "SamplerId" (toJSON @Word8))
      , (49, decodeField' "SamplerMode" (toJSON @Word8))
      , (50, decodeField' "SamplerRandomInterval" (toJSON @Word32))
      , (51, decodeField' "ClassId" (toJSON @Word8))
      , (52, decodeField' "MinimumTTL" (toJSON @Word8))
      , (53, decodeField' "MaximumTTL" (toJSON @Word8))
      , (54, decodeField' "FragmentIdentification" (toJSON @Word32))
      , (55, decodeField' "PostIpClassOfService" (toJSON @Word8))
      , (56, decodeField' "SourceMacAddress" (toJSON @MAC))
      , (57, decodeField' "PostDestinationMacAddress" (toJSON @MAC))
      , (58, decodeField' "VlanId" (toJSON @Word16))
      , (59, decodeField' "PostVlanId" (toJSON @Word16))
      , (60, decodeField' "IpVersion" (toJSON @Word8))
      , (61, decodeField' "FlowDirection" (toJSON @Word8))
      , (62, decodeField' "IpNextHopIPv6Address" (toJSON @Addr6))
      , (63, decodeField' "BgpNextHopIPv6Address" (toJSON @Addr6))
      , (64, decodeField' "Ipv6ExtensionHeaders" (toJSON @Word32))
      , (70, decodeField' "MplsTopLabelStackSection" (toJSON @Bytes))
      , (71, decodeField' "MplsLabelStackSection2" (toJSON @Bytes))
      , (72, decodeField' "MplsLabelStackSection3" (toJSON @Bytes))
      , (73, decodeField' "MplsLabelStackSection4" (toJSON @Bytes))
      , (74, decodeField' "MplsLabelStackSection5" (toJSON @Bytes))
      , (75, decodeField' "MplsLabelStackSection6" (toJSON @Bytes))
      , (76, decodeField' "MplsLabelStackSection7" (toJSON @Bytes))
      , (77, decodeField' "MplsLabelStackSection8" (toJSON @Bytes))
      , (78, decodeField' "MplsLabelStackSection9" (toJSON @Bytes))
      , (79, decodeField' "MplsLabelStackSection10" (toJSON @Bytes))
      , (80, decodeField' "DestinationMacAddress" (toJSON @MAC))
      , (81, decodeField' "PostSourceMacAddress" (toJSON @MAC))
      , (82, decodeField' "InterfaceName" (toJSON @Text))
      , (83, decodeField' "InterfaceDescription" (toJSON @Text))
      , (84, decodeField' "SamplerName" (toJSON @Text))
      , (85, decodeField' "OctetTotalCount" (toJSON @Word64))
      , (86, decodeField' "PacketTotalCount" (toJSON @Word64))
      , (87, decodeField' "FlagsAndSamplerId" (toJSON @Word32))
      , (88, decodeField' "FragmentOffset" (toJSON @Word16))
      , (89, decodeField' "ForwardingStatus" (toJSON @Word32))
      , (90, decodeField' "MplsVpnRouteDistinguisher" (toJSON @Bytes))
      , (91, decodeField' "MplsTopLabelPrefixLength" (toJSON @Word8))
      , (92, decodeField' "SrcTrafficIndex" (toJSON @Word32))
      , (93, decodeField' "DstTrafficIndex" (toJSON @Word32))
      , (94, decodeField' "ApplicationDescription" (toJSON @Text))
      , (95, decodeField' "ApplicationId" (toJSON @Bytes))
      , (96, decodeField' "ApplicationName" (toJSON @Text))
      , (98, decodeField' "PostIpDiffServCodePoint" (toJSON @Word8))
      , (99, decodeField' "MulticastReplicationFactor" (toJSON @Word32))
      , (100, decodeField' "ClassName" (toJSON @Text))
      , (101, decodeField' "ClassificationEngineId" (toJSON @Word8))
      , (102, decodeField' "Layer2packetSectionOffset" (toJSON @Word16))
      , (103, decodeField' "Layer2packetSectionSize" (toJSON @Word16))
      , (104, decodeField' "Layer2packetSectionData" (toJSON @Bytes))
      , (128, decodeField' "BgpNextAdjacentAsNumber" (toJSON @Word32))
      , (129, decodeField' "BgpPrevAdjacentAsNumber" (toJSON @Word32))
      , (130, decodeField' "ExporterIPv4Address" (toJSON @Addr4))
      , (131, decodeField' "ExporterIPv6Address" (toJSON @Addr6))
      , (132, decodeField' "DroppedOctetDeltaCount" (toJSON @Word64))
      , (133, decodeField' "DroppedPacketDeltaCount" (toJSON @Word64))
      , (134, decodeField' "DroppedOctetTotalCount" (toJSON @Word64))
      , (135, decodeField' "DroppedPacketTotalCount" (toJSON @Word64))
      , (136, decodeField' "FlowEndReason" (toJSON @Word8))
      , (137, decodeField' "CommonPropertiesId" (toJSON @Word64))
      , (138, decodeField' "ObservationPointId" (toJSON @Word64))
      , (139, decodeField' "IcmpTypeCodeIPv6" (toJSON @Word16))
      , (140, decodeField' "MplsTopLabelIPv6Address" (toJSON @Addr6))
      , (141, decodeField' "LineCardId" (toJSON @Word32))
      , (142, decodeField' "PortId" (toJSON @Word32))
      , (143, decodeField' "MeteringProcessId" (toJSON @Word32))
      , (144, decodeField' "ExportingProcessId" (toJSON @Word32))
      , (145, decodeField' "TemplateId" (toJSON @Word16))
      , (146, decodeField' "WlanChannelId" (toJSON @Word8))
      , (147, decodeField' "WlanSSID" (toJSON @Text))
      , (148, decodeField' "FlowId" (toJSON @Word64))
      , (149, decodeField' "ObservationDomainId" (toJSON @Word32))
      , (150, decodeField' "FlowStartSeconds" (toJSON @Word))
      , (151, decodeField' "FlowEndSeconds" (toJSON @Word))
      , (152, decodeField' "FlowStartMilliseconds" (toJSON @Word))
      , (153, decodeField' "FlowEndMilliseconds" (toJSON @Word))
      , (154, decodeField' "FlowStartMicroseconds" (toJSON @Word))
      , (155, decodeField' "FlowEndMicroseconds" (toJSON @Word))
      , (156, decodeField' "FlowStartNanoseconds" (toJSON @Word))
      , (157, decodeField' "FlowEndNanoseconds" (toJSON @Word))
      , (158, decodeField' "FlowStartDeltaMicroseconds" (toJSON @Word32))
      , (159, decodeField' "FlowEndDeltaMicroseconds" (toJSON @Word32))
      , (160, decodeField' "SystemInitTimeMilliseconds" (toJSON @Word))
      , (161, decodeField' "FlowDurationMilliseconds" (toJSON @Word32))
      , (162, decodeField' "FlowDurationMicroseconds" (toJSON @Word32))
      , (163, decodeField' "ObservedFlowTotalCount" (toJSON @Word64))
      , (164, decodeField' "IgnoredPacketTotalCount" (toJSON @Word64))
      , (165, decodeField' "IgnoredOctetTotalCount" (toJSON @Word64))
      , (166, decodeField' "NotSentFlowTotalCount" (toJSON @Word64))
      , (167, decodeField' "NotSentPacketTotalCount" (toJSON @Word64))
      , (168, decodeField' "NotSentOctetTotalCount" (toJSON @Word64))
      , (169, decodeField' "DestinationIPv6Prefix" (toJSON @Addr6))
      , (170, decodeField' "SourceIPv6Prefix" (toJSON @Addr6))
      , (171, decodeField' "PostOctetTotalCount" (toJSON @Word64))
      , (172, decodeField' "PostPacketTotalCount" (toJSON @Word64))
      , (173, decodeField' "FlowKeyIndicator" (toJSON @Word64))
      , (174, decodeField' "PostMCastPacketTotalCount" (toJSON @Word64))
      , (175, decodeField' "PostMCastOctetTotalCount" (toJSON @Word64))
      , (176, decodeField' "IcmpTypeIPv4" (toJSON @Word8))
      , (177, decodeField' "IcmpCodeIPv4" (toJSON @Word8))
      , (178, decodeField' "IcmpTypeIPv6" (toJSON @Word8))
      , (179, decodeField' "IcmpCodeIPv6" (toJSON @Word8))
      , (180, decodeField' "UdpSourcePort" (toJSON @Word16))
      , (181, decodeField' "UdpDestinationPort" (toJSON @Word16))
      , (182, decodeField' "TcpSourcePort" (toJSON @Word16))
      , (183, decodeField' "TcpDestinationPort" (toJSON @Word16))
      , (184, decodeField' "TcpSequenceNumber" (toJSON @Word32))
      , (185, decodeField' "TcpAcknowledgementNumber" (toJSON @Word32))
      , (186, decodeField' "TcpWindowSize" (toJSON @Word16))
      , (187, decodeField' "TcpUrgentPointer" (toJSON @Word16))
      , (188, decodeField' "TcpHeaderLength" (toJSON @Word8))
      , (189, decodeField' "IpHeaderLength" (toJSON @Word8))
      , (190, decodeField' "TotalLengthIPv4" (toJSON @Word16))
      , (191, decodeField' "PayloadLengthIPv6" (toJSON @Word16))
      , (192, decodeField' "IpTTL" (toJSON @Word8))
      , (193, decodeField' "NextHeaderIPv6" (toJSON @Word8))
      , (194, decodeField' "MplsPayloadLength" (toJSON @Word32))
      , (195, decodeField' "IpDiffServCodePoint" (toJSON @Word8))
      , (196, decodeField' "IpPrecedence" (toJSON @Word8))
      , (197, decodeField' "FragmentFlags" (toJSON @Word8))
      , (198, decodeField' "OctetDeltaSumOfSquares" (toJSON @Word64))
      , (199, decodeField' "OctetTotalSumOfSquares" (toJSON @Word64))
      , (200, decodeField' "MplsTopLabelTTL" (toJSON @Word8))
      , (201, decodeField' "MplsLabelStackLength" (toJSON @Word32))
      , (202, decodeField' "MplsLabelStackDepth" (toJSON @Word32))
      , (203, decodeField' "MplsTopLabelExp" (toJSON @Word8))
      , (204, decodeField' "IpPayloadLength" (toJSON @Word32))
      , (205, decodeField' "UdpMessageLength" (toJSON @Word16))
      , (206, decodeField' "IsMulticast" (toJSON @Word8))
      , (207, decodeField' "Ipv4IHL" (toJSON @Word8))
      , (208, decodeField' "Ipv4Options" (toJSON @Word32))
      , (209, decodeField' "TcpOptions" (toJSON @Word64))
      , (210, decodeField' "PaddingOctets" (toJSON @Bytes))
      , (211, decodeField' "CollectorIPv4Address" (toJSON @Addr4))
      , (212, decodeField' "CollectorIPv6Address" (toJSON @Addr6))
      , (213, decodeField' "ExportInterface" (toJSON @Word32))
      , (214, decodeField' "ExportProtocolVersion" (toJSON @Word8))
      , (215, decodeField' "ExportTransportProtocol" (toJSON @Word8))
      , (216, decodeField' "CollectorTransportPort" (toJSON @Word16))
      , (217, decodeField' "ExporterTransportPort" (toJSON @Word16))
      , (218, decodeField' "TcpSynTotalCount" (toJSON @Word64))
      , (219, decodeField' "TcpFinTotalCount" (toJSON @Word64))
      , (220, decodeField' "TcpRstTotalCount" (toJSON @Word64))
      , (221, decodeField' "TcpPshTotalCount" (toJSON @Word64))
      , (222, decodeField' "TcpAckTotalCount" (toJSON @Word64))
      , (223, decodeField' "TcpUrgTotalCount" (toJSON @Word64))
      , (224, decodeField' "IpTotalLength" (toJSON @Word64))
      , (225, decodeField' "PostNATSourceIPv4Address" (toJSON @Addr4))
      , (226, decodeField' "PostNATDestinationIPv4Address" (toJSON @Addr4))
      , (227, decodeField' "PostNAPTSourceTransportPort" (toJSON @Word16))
      , (228, decodeField' "PostNAPTDestinationTransportPort" (toJSON @Word16))
      , (229, decodeField' "NatOriginatingAddressRealm" (toJSON @Word8))
      , (230, decodeField' "NatEvent" (toJSON @Word8))
      , (231, decodeField' "InitiatorOctets" (toJSON @Word64))
      , (232, decodeField' "ResponderOctets" (toJSON @Word64))
      , (233, decodeField' "FirewallEvent" (toJSON @Word8))
      , (234, decodeField' "IngressVRFID" (toJSON @Word32))
      , (235, decodeField' "EgressVRFID" (toJSON @Word32))
      , (236, decodeField' "VRFname" (toJSON @Text))
      , (237, decodeField' "PostMplsTopLabelExp" (toJSON @Word8))
      , (238, decodeField' "TcpWindowScale" (toJSON @Word16))
      , (239, decodeField' "BiflowDirection" (toJSON @Word8))
      , (240, decodeField' "EthernetHeaderLength" (toJSON @Word8))
      , (241, decodeField' "EthernetPayloadLength" (toJSON @Word16))
      , (242, decodeField' "EthernetTotalLength" (toJSON @Word16))
      , (243, decodeField' "Dot1qVlanId" (toJSON @Word16))
      , (244, decodeField' "Dot1qPriority" (toJSON @Word8))
      , (245, decodeField' "Dot1qCustomerVlanId" (toJSON @Word16))
      , (246, decodeField' "Dot1qCustomerPriority" (toJSON @Word8))
      , (247, decodeField' "MetroEvcId" (toJSON @Text))
      , (248, decodeField' "MetroEvcType" (toJSON @Word8))
      , (249, decodeField' "PseudoWireId" (toJSON @Word32))
      , (250, decodeField' "PseudoWireType" (toJSON @Word16))
      , (251, decodeField' "PseudoWireControlWord" (toJSON @Word32))
      , (252, decodeField' "IngressPhysicalInterface" (toJSON @Word32))
      , (253, decodeField' "EgressPhysicalInterface" (toJSON @Word32))
      , (254, decodeField' "PostDot1qVlanId" (toJSON @Word16))
      , (255, decodeField' "PostDot1qCustomerVlanId" (toJSON @Word16))
      , (256, decodeField' "EthernetType" (toJSON @Word16))
      , (257, decodeField' "PostIpPrecedence" (toJSON @Word8))
      , (258, decodeField' "CollectionTimeMilliseconds" (toJSON @Word))
      , (259, decodeField' "ExportSctpStreamId" (toJSON @Word16))
      , (260, decodeField' "MaxExportSeconds" (toJSON @Word))
      , (261, decodeField' "MaxFlowEndSeconds" (toJSON @Word))
      , (262, decodeField' "MessageMD5Checksum" (toJSON @Bytes))
      , (263, decodeField' "MessageScope" (toJSON @Word8))
      , (264, decodeField' "MinExportSeconds" (toJSON @Word))
      , (265, decodeField' "MinFlowStartSeconds" (toJSON @Word))
      , (266, decodeField' "OpaqueOctets" (toJSON @Bytes))
      , (267, decodeField' "SessionScope" (toJSON @Word8))
      , (268, decodeField' "MaxFlowEndMicroseconds" (toJSON @Word))
      , (269, decodeField' "MaxFlowEndMilliseconds" (toJSON @Word))
      , (270, decodeField' "MaxFlowEndNanoseconds" (toJSON @Word))
      , (271, decodeField' "MinFlowStartMicroseconds" (toJSON @Word))
      , (272, decodeField' "MinFlowStartMilliseconds" (toJSON @Word))
      , (273, decodeField' "MinFlowStartNanoseconds" (toJSON @Word))
      , (274, decodeField' "CollectorCertificate" (toJSON @Bytes))
      , (275, decodeField' "ExporterCertificate" (toJSON @Bytes))
      , (276, decodeField' "DataRecordsReliability" (toJSON @Bool))
      , (277, decodeField' "ObservationPointType" (toJSON @Word8))
      , (278, decodeField' "NewConnectionDeltaCount" (toJSON @Word32))
      , (279, decodeField' "ConnectionSumDurationSeconds" (toJSON @Word64))
      , (280, decodeField' "ConnectionTransactionId" (toJSON @Word64))
      , (281, decodeField' "PostNATSourceIPv6Address" (toJSON @Addr6))
      , (282, decodeField' "PostNATDestinationIPv6Address" (toJSON @Addr6))
      , (283, decodeField' "NatPoolId" (toJSON @Word32))
      , (284, decodeField' "NatPoolName" (toJSON @Text))
      , (285, decodeField' "AnonymizationFlags" (toJSON @Word16))
      , (286, decodeField' "AnonymizationTechnique" (toJSON @Word16))
      , (287, decodeField' "InformationElementIndex" (toJSON @Word16))
      , (288, decodeField' "P2pTechnology" (toJSON @Text))
      , (289, decodeField' "TunnelTechnology" (toJSON @Text))
      , (290, decodeField' "EncryptedTechnology" (toJSON @Text))
      , (294, decodeField' "BgpValidityState" (toJSON @Word8))
      , (295, decodeField' "IPSecSPI" (toJSON @Word32))
      , (296, decodeField' "GreKey" (toJSON @Word32))
      , (297, decodeField' "NatType" (toJSON @Word8))
      , (298, decodeField' "InitiatorPackets" (toJSON @Word64))
      , (299, decodeField' "ResponderPackets" (toJSON @Word64))
      , (300, decodeField' "ObservationDomainName" (toJSON @Text))
      , (301, decodeField' "SelectionSequenceId" (toJSON @Word64))
      , (302, decodeField' "SelectorId" (toJSON @Word64))
      , (303, decodeField' "InformationElementId" (toJSON @Word16))
      , (304, decodeField' "SelectorAlgorithm" (toJSON @Word16))
      , (305, decodeField' "SamplingPacketInterval" (toJSON @Word32))
      , (306, decodeField' "SamplingPacketSpace" (toJSON @Word32))
      , (307, decodeField' "SamplingTimeInterval" (toJSON @Word32))
      , (308, decodeField' "SamplingTimeSpace" (toJSON @Word32))
      , (309, decodeField' "SamplingSize" (toJSON @Word32))
      , (310, decodeField' "SamplingPopulation" (toJSON @Word32))
      , (311, decodeField' "SamplingProbability" (toJSON @Double))
      , (312, decodeField' "DataLinkFrameSize" (toJSON @Word16))
      , (313, decodeField' "IpHeaderPacketSection" (toJSON @Bytes))
      , (314, decodeField' "IpPayloadPacketSection" (toJSON @Bytes))
      , (315, decodeField' "DataLinkFrameSection" (toJSON @Bytes))
      , (316, decodeField' "MplsLabelStackSection" (toJSON @Bytes))
      , (317, decodeField' "MplsPayloadPacketSection" (toJSON @Bytes))
      , (318, decodeField' "SelectorIdTotalPktsObserved" (toJSON @Word64))
      , (319, decodeField' "SelectorIdTotalPktsSelected" (toJSON @Word64))
      , (320, decodeField' "AbsoluteError" (toJSON @Double))
      , (321, decodeField' "RelativeError" (toJSON @Double))
      , (322, decodeField' "ObservationTimeSeconds" (toJSON @Word))
      , (323, decodeField' "ObservationTimeMilliseconds" (toJSON @Word))
      , (324, decodeField' "ObservationTimeMicroseconds" (toJSON @Word))
      , (325, decodeField' "ObservationTimeNanoseconds" (toJSON @Word))
      , (326, decodeField' "DigestHashValue" (toJSON @Word64))
      , (327, decodeField' "HashIPPayloadOffset" (toJSON @Word64))
      , (328, decodeField' "HashIPPayloadSize" (toJSON @Word64))
      , (329, decodeField' "HashOutputRangeMin" (toJSON @Word64))
      , (330, decodeField' "HashOutputRangeMax" (toJSON @Word64))
      , (331, decodeField' "HashSelectedRangeMin" (toJSON @Word64))
      , (332, decodeField' "HashSelectedRangeMax" (toJSON @Word64))
      , (333, decodeField' "HashDigestOutput" (toJSON @Bool))
      , (334, decodeField' "HashInitialiserValue" (toJSON @Word64))
      , (335, decodeField' "SelectorName" (toJSON @Text))
      , (336, decodeField' "UpperCILimit" (toJSON @Double))
      , (337, decodeField' "LowerCILimit" (toJSON @Double))
      , (338, decodeField' "ConfidenceLevel" (toJSON @Double))
      , (339, decodeField' "InformationElementDataType" (toJSON @Word8))
      , (340, decodeField' "InformationElementDescription" (toJSON @Text))
      , (341, decodeField' "InformationElementName" (toJSON @Text))
      , (342, decodeField' "InformationElementRangeBegin" (toJSON @Word64))
      , (343, decodeField' "InformationElementRangeEnd" (toJSON @Word64))
      , (344, decodeField' "InformationElementSemantics" (toJSON @Word8))
      , (345, decodeField' "InformationElementUnits" (toJSON @Word16))
      , (346, decodeField' "PrivateEnterpriseNumber" (toJSON @Word32))
      , (347, decodeField' "VirtualStationInterfaceId" (toJSON @Bytes))
      , (348, decodeField' "VirtualStationInterfaceName" (toJSON @Text))
      , (349, decodeField' "VirtualStationUUID" (toJSON @Bytes))
      , (350, decodeField' "VirtualStationName" (toJSON @Text))
      , (351, decodeField' "Layer2SegmentId" (toJSON @Word64))
      , (352, decodeField' "Layer2OctetDeltaCount" (toJSON @Word64))
      , (353, decodeField' "Layer2OctetTotalCount" (toJSON @Word64))
      , (354, decodeField' "IngressUnicastPacketTotalCount" (toJSON @Word64))
      , (355, decodeField' "IngressMulticastPacketTotalCount" (toJSON @Word64))
      , (356, decodeField' "IngressBroadcastPacketTotalCount" (toJSON @Word64))
      , (357, decodeField' "EgressUnicastPacketTotalCount" (toJSON @Word64))
      , (358, decodeField' "EgressBroadcastPacketTotalCount" (toJSON @Word64))
      , (359, decodeField' "MonitoringIntervalStartMilliSeconds" (toJSON @Word))
      , (360, decodeField' "MonitoringIntervalEndMilliSeconds" (toJSON @Word))
      , (361, decodeField' "PortRangeStart" (toJSON @Word16))
      , (362, decodeField' "PortRangeEnd" (toJSON @Word16))
      , (363, decodeField' "PortRangeStepSize" (toJSON @Word16))
      , (364, decodeField' "PortRangeNumPorts" (toJSON @Word16))
      , (365, decodeField' "StaMacAddress" (toJSON @MAC))
      , (366, decodeField' "StaIPv4Address" (toJSON @Addr4))
      , (367, decodeField' "WtpMacAddress" (toJSON @MAC))
      , (368, decodeField' "IngressInterfaceType" (toJSON @Word32))
      , (369, decodeField' "EgressInterfaceType" (toJSON @Word32))
      , (370, decodeField' "RtpSequenceNumber" (toJSON @Word16))
      , (371, decodeField' "UserName" (toJSON @Text))
      , (372, decodeField' "ApplicationCategoryName" (toJSON @Text))
      , (373, decodeField' "ApplicationSubCategoryName" (toJSON @Text))
      , (374, decodeField' "ApplicationGroupName" (toJSON @Text))
      , (375, decodeField' "OriginalFlowsPresent" (toJSON @Word64))
      , (376, decodeField' "OriginalFlowsInitiated" (toJSON @Word64))
      , (377, decodeField' "OriginalFlowsCompleted" (toJSON @Word64))
      , (378, decodeField' "DistinctCountOfSourceIPAddress" (toJSON @Word64))
      , (379, decodeField' "DistinctCountOfDestinationIPAddress" (toJSON @Word64))
      , (380, decodeField' "DistinctCountOfSourceIPv4Address" (toJSON @Word32))
      , (381, decodeField' "DistinctCountOfDestinationIPv4Address" (toJSON @Word32))
      , (382, decodeField' "DistinctCountOfSourceIPv6Address" (toJSON @Word64))
      , (383, decodeField' "DistinctCountOfDestinationIPv6Address" (toJSON @Word64))
      , (384, decodeField' "ValueDistributionMethod" (toJSON @Word8))
      , (385, decodeField' "Rfc3550JitterMilliseconds" (toJSON @Word32))
      , (386, decodeField' "Rfc3550JitterMicroseconds" (toJSON @Word32))
      , (387, decodeField' "Rfc3550JitterNanoseconds" (toJSON @Word32))
      , (388, decodeField' "Dot1qDEI" (toJSON @Bool))
      , (389, decodeField' "Dot1qCustomerDEI" (toJSON @Bool))
      , (390, decodeField' "FlowSelectorAlgorithm" (toJSON @Word16))
      , (391, decodeField' "FlowSelectedOctetDeltaCount" (toJSON @Word64))
      , (392, decodeField' "FlowSelectedPacketDeltaCount" (toJSON @Word64))
      , (393, decodeField' "FlowSelectedFlowDeltaCount" (toJSON @Word64))
      , (394, decodeField' "SelectorIDTotalFlowsObserved" (toJSON @Word64))
      , (395, decodeField' "SelectorIDTotalFlowsSelected" (toJSON @Word64))
      , (396, decodeField' "SamplingFlowInterval" (toJSON @Word64))
      , (397, decodeField' "SamplingFlowSpacing" (toJSON @Word64))
      , (398, decodeField' "FlowSamplingTimeInterval" (toJSON @Word64))
      , (399, decodeField' "FlowSamplingTimeSpacing" (toJSON @Word64))
      , (400, decodeField' "HashFlowDomain" (toJSON @Word16))
      , (401, decodeField' "TransportOctetDeltaCount" (toJSON @Word64))
      , (402, decodeField' "TransportPacketDeltaCount" (toJSON @Word64))
      , (403, decodeField' "OriginalExporterIPv4Address" (toJSON @Addr4))
      , (404, decodeField' "OriginalExporterIPv6Address" (toJSON @Addr6))
      , (405, decodeField' "OriginalObservationDomainId" (toJSON @Word32))
      , (406, decodeField' "IntermediateProcessId" (toJSON @Word32))
      , (407, decodeField' "IgnoredDataRecordTotalCount" (toJSON @Word64))
      , (408, decodeField' "DataLinkFrameType" (toJSON @Word16))
      , (409, decodeField' "SectionOffset" (toJSON @Word16))
      , (410, decodeField' "SectionExportedOctets" (toJSON @Word16))
      , (411, decodeField' "Dot1qServiceInstanceTag" (toJSON @Bytes))
      , (412, decodeField' "Dot1qServiceInstanceId" (toJSON @Word32))
      , (413, decodeField' "Dot1qServiceInstancePriority" (toJSON @Word8))
      , (414, decodeField' "Dot1qCustomerSourceMacAddress" (toJSON @MAC))
      , (415, decodeField' "Dot1qCustomerDestinationMacAddress" (toJSON @MAC))
      , (417, decodeField' "PostLayer2OctetDeltaCount" (toJSON @Word64))
      , (418, decodeField' "PostMCastLayer2OctetDeltaCount" (toJSON @Word64))
      , (420, decodeField' "PostLayer2OctetTotalCount" (toJSON @Word64))
      , (421, decodeField' "PostMCastLayer2OctetTotalCount" (toJSON @Word64))
      , (422, decodeField' "MinimumLayer2TotalLength" (toJSON @Word64))
      , (423, decodeField' "MaximumLayer2TotalLength" (toJSON @Word64))
      , (424, decodeField' "DroppedLayer2OctetDeltaCount" (toJSON @Word64))
      , (425, decodeField' "DroppedLayer2OctetTotalCount" (toJSON @Word64))
      , (426, decodeField' "IgnoredLayer2OctetTotalCount" (toJSON @Word64))
      , (427, decodeField' "NotSentLayer2OctetTotalCount" (toJSON @Word64))
      , (428, decodeField' "Layer2OctetDeltaSumOfSquares" (toJSON @Word64))
      , (429, decodeField' "Layer2OctetTotalSumOfSquares" (toJSON @Word64))
      , (430, decodeField' "Layer2FrameDeltaCount" (toJSON @Word64))
      , (431, decodeField' "Layer2FrameTotalCount" (toJSON @Word64))
      , (432, decodeField' "PseudoWireDestinationIPv4Address" (toJSON @Addr4))
      , (433, decodeField' "IgnoredLayer2FrameTotalCount" (toJSON @Word64))
      , (434, decodeField' "MibObjectValueInteger" (toJSON @Int64))
      , (435, decodeField' "MibObjectValueOctetString" (toJSON @Bytes))
      , (436, decodeField' "MibObjectValueOID" (toJSON @Bytes))
      , (437, decodeField' "MibObjectValueBits" (toJSON @Bytes))
      , (438, decodeField' "MibObjectValueIPAddress" (toJSON @Addr4))
      , (439, decodeField' "MibObjectValueCounter" (toJSON @Word64))
      , (440, decodeField' "MibObjectValueGauge" (toJSON @Word32))
      , (441, decodeField' "MibObjectValueTimeTicks" (toJSON @Word32))
      , (442, decodeField' "MibObjectValueUnsigned" (toJSON @Word64))
      , (445, decodeField' "MibObjectIdentifier" (toJSON @Bytes))
      , (446, decodeField' "MibSubIdentifier" (toJSON @Word32))
      , (447, decodeField' "MibIndexIndicator" (toJSON @Word64))
      , (448, decodeField' "MibCaptureTimeSemantics" (toJSON @Word8))
      , (449, decodeField' "MibContextEngineID" (toJSON @Bytes))
      , (450, decodeField' "MibContextName" (toJSON @Text))
      , (451, decodeField' "MibObjectName" (toJSON @Text))
      , (452, decodeField' "MibObjectDescription" (toJSON @Text))
      , (453, decodeField' "MibObjectSyntax" (toJSON @Text))
      , (454, decodeField' "MibModuleName" (toJSON @Text))
      , (455, decodeField' "MobileIMSI" (toJSON @Text))
      , (456, decodeField' "MobileMSISDN" (toJSON @Text))
      , (457, decodeField' "HttpStatusCode" (toJSON @Word16))
      ]


-- vim:set ft=haskell sw=2 ts=2 et:
