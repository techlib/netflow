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
  import BasePrelude hiding (union, lookup)

  import Data.MAC (MAC, toMAC)
  import Data.IP (IPv4, IPv6, toIPv4, toIPv6b)
  import Data.Text.Encoding (decodeUtf8, decodeUtf8')
  import Data.Serialize.Get
  import Data.Serialize.IEEE754

  import Data.HashMap.Lazy (HashMap, singleton, fromList, lookup)
  import Data.ByteString (ByteString)
  import Data.Text (Text)
  import Data.Aeson

  import qualified Data.ByteString.Base64 as Base64
  import qualified Data.ByteString as BS


  decodeField :: Word16 -> ByteString -> Object
  decodeField fid bs = case lookup fid decoders of
                           Just dec -> dec bs
                           Nothing  -> decodeOther fid bs


  decodeOther :: Word16 -> ByteString -> Object
  decodeOther fid bs = singleton name value
    where name = fromString $ show fid
          value = toJSON @Bytes $ decodeAs bs


  class DecodeAs a where
    decodeAs :: ByteString -> a


  instance DecodeAs Bytes where
    decodeAs = Bytes


  instance DecodeAs Bool where
    decodeAs bs = case (roll bs :: Word) of
                    1 -> True
                    _ -> False


  instance DecodeAs Word where
    decodeAs = roll


  instance DecodeAs Word8 where
    decodeAs = roll


  instance DecodeAs Word16 where
    decodeAs = roll


  instance DecodeAs Word32 where
    decodeAs = roll


  instance DecodeAs Word64 where
    decodeAs = roll


  instance DecodeAs Int64 where
    decodeAs bs = case runGet getInt64be bs of
                    Left _e -> 0
                    Right v -> v


  instance DecodeAs Text where
    decodeAs bs = case decodeUtf8' bs of
                    Left _e -> ""
                    Right v -> v


  instance DecodeAs Double where
    decodeAs bs = case runGet getFloat64be bs of
                    Left _e -> 0.0
                    Right v -> v


  instance DecodeAs Addr4 where
    decodeAs bs = case BS.length bs of
                    4 -> Addr4 $ toIPv4 $ map fromIntegral $ BS.unpack bs
                    _ -> Addr4 $ read "0.0.0.0"


  instance DecodeAs Addr6 where
    decodeAs bs = case BS.length bs of
                    16 -> Addr6 $ toIPv6b $ map fromIntegral $ BS.unpack bs
                    _x -> Addr6 $ read "::"


  instance DecodeAs MAC where
    decodeAs bs = case BS.length bs of
                    6 -> toMAC $ BS.unpack bs
                    _ -> toMAC [0, 0, 0, 0, 0, 0]


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
      [ (1, singleton "OctetDeltaCount" . toJSON @Word64 . decodeAs)
      , (2, singleton "PacketDeltaCount" . toJSON @Word64 . decodeAs)
      , (3, singleton "DeltaFlowCount" . toJSON @Word64 . decodeAs)
      , (4, singleton "ProtocolIdentifier" . toJSON @Word8 . decodeAs)
      , (5, singleton "IpClassOfService" . toJSON @Word8 . decodeAs)
      , (6, singleton "TcpControlBits" . toJSON @Word16 . decodeAs)
      , (7, singleton "SourceTransportPort" . toJSON @Word16 . decodeAs)
      , (8, singleton "SourceIPv4Address" . toJSON @Addr4 . decodeAs)
      , (9, singleton "SourceIPv4PrefixLength" . toJSON @Word8 . decodeAs)
      , (10, singleton "IngressInterface" . toJSON @Word32 . decodeAs)
      , (11, singleton "DestinationTransportPort" . toJSON @Word16 . decodeAs)
      , (12, singleton "DestinationIPv4Address" . toJSON @Addr4 . decodeAs)
      , (13, singleton "DestinationIPv4PrefixLength" . toJSON @Word8 . decodeAs)
      , (14, singleton "EgressInterface" . toJSON @Word32 . decodeAs)
      , (15, singleton "IpNextHopIPv4Address" . toJSON @Addr4 . decodeAs)
      , (16, singleton "BgpSourceAsNumber" . toJSON @Word32 . decodeAs)
      , (17, singleton "BgpDestinationAsNumber" . toJSON @Word32 . decodeAs)
      , (18, singleton "BgpNextHopIPv4Address" . toJSON @Addr4 . decodeAs)
      , (19, singleton "PostMCastPacketDeltaCount" . toJSON @Word64 . decodeAs)
      , (20, singleton "PostMCastOctetDeltaCount" . toJSON @Word64 . decodeAs)
      , (21, singleton "FlowEndSysUpTime" . toJSON @Word32 . decodeAs)
      , (22, singleton "FlowStartSysUpTime" . toJSON @Word32 . decodeAs)
      , (23, singleton "PostOctetDeltaCount" . toJSON @Word64 . decodeAs)
      , (24, singleton "PostPacketDeltaCount" . toJSON @Word64 . decodeAs)
      , (25, singleton "MinimumIpTotalLength" . toJSON @Word64 . decodeAs)
      , (26, singleton "MaximumIpTotalLength" . toJSON @Word64 . decodeAs)
      , (27, singleton "SourceIPv6Address" . toJSON @Addr6 . decodeAs)
      , (28, singleton "DestinationIPv6Address" . toJSON @Addr6 . decodeAs)
      , (29, singleton "SourceIPv6PrefixLength" . toJSON @Word8 . decodeAs)
      , (30, singleton "DestinationIPv6PrefixLength" . toJSON @Word8 . decodeAs)
      , (31, singleton "FlowLabelIPv6" . toJSON @Word32 . decodeAs)
      , (32, singleton "IcmpTypeCodeIPv4" . toJSON @Word16 . decodeAs)
      , (33, singleton "IgmpType" . toJSON @Word8 . decodeAs)
      , (34, singleton "SamplingInterval" . toJSON @Word32 . decodeAs)
      , (35, singleton "SamplingAlgorithm" . toJSON @Word8 . decodeAs)
      , (36, singleton "FlowActiveTimeout" . toJSON @Word16 . decodeAs)
      , (37, singleton "FlowIdleTimeout" . toJSON @Word16 . decodeAs)
      , (38, singleton "EngineType" . toJSON @Word8 . decodeAs)
      , (39, singleton "EngineId" . toJSON @Word8 . decodeAs)
      , (40, singleton "ExportedOctetTotalCount" . toJSON @Word64 . decodeAs)
      , (41, singleton "ExportedMessageTotalCount" . toJSON @Word64 . decodeAs)
      , (42, singleton "ExportedFlowRecordTotalCount" . toJSON @Word64 . decodeAs)
      , (43, singleton "Ipv4RouterSc" . toJSON @Addr4 . decodeAs)
      , (44, singleton "SourceIPv4Prefix" . toJSON @Addr4 . decodeAs)
      , (45, singleton "DestinationIPv4Prefix" . toJSON @Addr4 . decodeAs)
      , (46, singleton "MplsTopLabelType" . toJSON @Word8 . decodeAs)
      , (47, singleton "MplsTopLabelIPv4Address" . toJSON @Addr4 . decodeAs)
      , (48, singleton "SamplerId" . toJSON @Word8 . decodeAs)
      , (49, singleton "SamplerMode" . toJSON @Word8 . decodeAs)
      , (50, singleton "SamplerRandomInterval" . toJSON @Word32 . decodeAs)
      , (51, singleton "ClassId" . toJSON @Word8 . decodeAs)
      , (52, singleton "MinimumTTL" . toJSON @Word8 . decodeAs)
      , (53, singleton "MaximumTTL" . toJSON @Word8 . decodeAs)
      , (54, singleton "FragmentIdentification" . toJSON @Word32 . decodeAs)
      , (55, singleton "PostIpClassOfService" . toJSON @Word8 . decodeAs)
      , (56, singleton "SourceMacAddress" . toJSON @MAC . decodeAs)
      , (57, singleton "PostDestinationMacAddress" . toJSON @MAC . decodeAs)
      , (58, singleton "VlanId" . toJSON @Word16 . decodeAs)
      , (59, singleton "PostVlanId" . toJSON @Word16 . decodeAs)
      , (60, singleton "IpVersion" . toJSON @Word8 . decodeAs)
      , (61, singleton "FlowDirection" . toJSON @Word8 . decodeAs)
      , (62, singleton "IpNextHopIPv6Address" . toJSON @Addr6 . decodeAs)
      , (63, singleton "BgpNextHopIPv6Address" . toJSON @Addr6 . decodeAs)
      , (64, singleton "Ipv6ExtensionHeaders" . toJSON @Word32 . decodeAs)
      , (70, singleton "MplsTopLabelStackSection" . toJSON @Bytes . decodeAs)
      , (71, singleton "MplsLabelStackSection2" . toJSON @Bytes . decodeAs)
      , (72, singleton "MplsLabelStackSection3" . toJSON @Bytes . decodeAs)
      , (73, singleton "MplsLabelStackSection4" . toJSON @Bytes . decodeAs)
      , (74, singleton "MplsLabelStackSection5" . toJSON @Bytes . decodeAs)
      , (75, singleton "MplsLabelStackSection6" . toJSON @Bytes . decodeAs)
      , (76, singleton "MplsLabelStackSection7" . toJSON @Bytes . decodeAs)
      , (77, singleton "MplsLabelStackSection8" . toJSON @Bytes . decodeAs)
      , (78, singleton "MplsLabelStackSection9" . toJSON @Bytes . decodeAs)
      , (79, singleton "MplsLabelStackSection10" . toJSON @Bytes . decodeAs)
      , (80, singleton "DestinationMacAddress" . toJSON @MAC . decodeAs)
      , (81, singleton "PostSourceMacAddress" . toJSON @MAC . decodeAs)
      , (82, singleton "InterfaceName" . toJSON @Text . decodeAs)
      , (83, singleton "InterfaceDescription" . toJSON @Text . decodeAs)
      , (84, singleton "SamplerName" . toJSON @Text . decodeAs)
      , (85, singleton "OctetTotalCount" . toJSON @Word64 . decodeAs)
      , (86, singleton "PacketTotalCount" . toJSON @Word64 . decodeAs)
      , (87, singleton "FlagsAndSamplerId" . toJSON @Word32 . decodeAs)
      , (88, singleton "FragmentOffset" . toJSON @Word16 . decodeAs)
      , (89, singleton "ForwardingStatus" . toJSON @Word32 . decodeAs)
      , (90, singleton "MplsVpnRouteDistinguisher" . toJSON @Bytes . decodeAs)
      , (91, singleton "MplsTopLabelPrefixLength" . toJSON @Word8 . decodeAs)
      , (92, singleton "SrcTrafficIndex" . toJSON @Word32 . decodeAs)
      , (93, singleton "DstTrafficIndex" . toJSON @Word32 . decodeAs)
      , (94, singleton "ApplicationDescription" . toJSON @Text . decodeAs)
      , (95, singleton "ApplicationId" . toJSON @Bytes . decodeAs)
      , (96, singleton "ApplicationName" . toJSON @Text . decodeAs)
      , (98, singleton "PostIpDiffServCodePoint" . toJSON @Word8 . decodeAs)
      , (99, singleton "MulticastReplicationFactor" . toJSON @Word32 . decodeAs)
      , (100, singleton "ClassName" . toJSON @Text . decodeAs)
      , (101, singleton "ClassificationEngineId" . toJSON @Word8 . decodeAs)
      , (102, singleton "Layer2packetSectionOffset" . toJSON @Word16 . decodeAs)
      , (103, singleton "Layer2packetSectionSize" . toJSON @Word16 . decodeAs)
      , (104, singleton "Layer2packetSectionData" . toJSON @Bytes . decodeAs)
      , (128, singleton "BgpNextAdjacentAsNumber" . toJSON @Word32 . decodeAs)
      , (129, singleton "BgpPrevAdjacentAsNumber" . toJSON @Word32 . decodeAs)
      , (130, singleton "ExporterIPv4Address" . toJSON @Addr4 . decodeAs)
      , (131, singleton "ExporterIPv6Address" . toJSON @Addr6 . decodeAs)
      , (132, singleton "DroppedOctetDeltaCount" . toJSON @Word64 . decodeAs)
      , (133, singleton "DroppedPacketDeltaCount" . toJSON @Word64 . decodeAs)
      , (134, singleton "DroppedOctetTotalCount" . toJSON @Word64 . decodeAs)
      , (135, singleton "DroppedPacketTotalCount" . toJSON @Word64 . decodeAs)
      , (136, singleton "FlowEndReason" . toJSON @Word8 . decodeAs)
      , (137, singleton "CommonPropertiesId" . toJSON @Word64 . decodeAs)
      , (138, singleton "ObservationPointId" . toJSON @Word64 . decodeAs)
      , (139, singleton "IcmpTypeCodeIPv6" . toJSON @Word16 . decodeAs)
      , (140, singleton "MplsTopLabelIPv6Address" . toJSON @Addr6 . decodeAs)
      , (141, singleton "LineCardId" . toJSON @Word32 . decodeAs)
      , (142, singleton "PortId" . toJSON @Word32 . decodeAs)
      , (143, singleton "MeteringProcessId" . toJSON @Word32 . decodeAs)
      , (144, singleton "ExportingProcessId" . toJSON @Word32 . decodeAs)
      , (145, singleton "TemplateId" . toJSON @Word16 . decodeAs)
      , (146, singleton "WlanChannelId" . toJSON @Word8 . decodeAs)
      , (147, singleton "WlanSSID" . toJSON @Text . decodeAs)
      , (148, singleton "FlowId" . toJSON @Word64 . decodeAs)
      , (149, singleton "ObservationDomainId" . toJSON @Word32 . decodeAs)
      , (150, singleton "FlowStartSeconds" . toJSON @Word . decodeAs)
      , (151, singleton "FlowEndSeconds" . toJSON @Word . decodeAs)
      , (152, singleton "FlowStartMilliseconds" . toJSON @Word . decodeAs)
      , (153, singleton "FlowEndMilliseconds" . toJSON @Word . decodeAs)
      , (154, singleton "FlowStartMicroseconds" . toJSON @Word . decodeAs)
      , (155, singleton "FlowEndMicroseconds" . toJSON @Word . decodeAs)
      , (156, singleton "FlowStartNanoseconds" . toJSON @Word . decodeAs)
      , (157, singleton "FlowEndNanoseconds" . toJSON @Word . decodeAs)
      , (158, singleton "FlowStartDeltaMicroseconds" . toJSON @Word32 . decodeAs)
      , (159, singleton "FlowEndDeltaMicroseconds" . toJSON @Word32 . decodeAs)
      , (160, singleton "SystemInitTimeMilliseconds" . toJSON @Word . decodeAs)
      , (161, singleton "FlowDurationMilliseconds" . toJSON @Word32 . decodeAs)
      , (162, singleton "FlowDurationMicroseconds" . toJSON @Word32 . decodeAs)
      , (163, singleton "ObservedFlowTotalCount" . toJSON @Word64 . decodeAs)
      , (164, singleton "IgnoredPacketTotalCount" . toJSON @Word64 . decodeAs)
      , (165, singleton "IgnoredOctetTotalCount" . toJSON @Word64 . decodeAs)
      , (166, singleton "NotSentFlowTotalCount" . toJSON @Word64 . decodeAs)
      , (167, singleton "NotSentPacketTotalCount" . toJSON @Word64 . decodeAs)
      , (168, singleton "NotSentOctetTotalCount" . toJSON @Word64 . decodeAs)
      , (169, singleton "DestinationIPv6Prefix" . toJSON @Addr6 . decodeAs)
      , (170, singleton "SourceIPv6Prefix" . toJSON @Addr6 . decodeAs)
      , (171, singleton "PostOctetTotalCount" . toJSON @Word64 . decodeAs)
      , (172, singleton "PostPacketTotalCount" . toJSON @Word64 . decodeAs)
      , (173, singleton "FlowKeyIndicator" . toJSON @Word64 . decodeAs)
      , (174, singleton "PostMCastPacketTotalCount" . toJSON @Word64 . decodeAs)
      , (175, singleton "PostMCastOctetTotalCount" . toJSON @Word64 . decodeAs)
      , (176, singleton "IcmpTypeIPv4" . toJSON @Word8 . decodeAs)
      , (177, singleton "IcmpCodeIPv4" . toJSON @Word8 . decodeAs)
      , (178, singleton "IcmpTypeIPv6" . toJSON @Word8 . decodeAs)
      , (179, singleton "IcmpCodeIPv6" . toJSON @Word8 . decodeAs)
      , (180, singleton "UdpSourcePort" . toJSON @Word16 . decodeAs)
      , (181, singleton "UdpDestinationPort" . toJSON @Word16 . decodeAs)
      , (182, singleton "TcpSourcePort" . toJSON @Word16 . decodeAs)
      , (183, singleton "TcpDestinationPort" . toJSON @Word16 . decodeAs)
      , (184, singleton "TcpSequenceNumber" . toJSON @Word32 . decodeAs)
      , (185, singleton "TcpAcknowledgementNumber" . toJSON @Word32 . decodeAs)
      , (186, singleton "TcpWindowSize" . toJSON @Word16 . decodeAs)
      , (187, singleton "TcpUrgentPointer" . toJSON @Word16 . decodeAs)
      , (188, singleton "TcpHeaderLength" . toJSON @Word8 . decodeAs)
      , (189, singleton "IpHeaderLength" . toJSON @Word8 . decodeAs)
      , (190, singleton "TotalLengthIPv4" . toJSON @Word16 . decodeAs)
      , (191, singleton "PayloadLengthIPv6" . toJSON @Word16 . decodeAs)
      , (192, singleton "IpTTL" . toJSON @Word8 . decodeAs)
      , (193, singleton "NextHeaderIPv6" . toJSON @Word8 . decodeAs)
      , (194, singleton "MplsPayloadLength" . toJSON @Word32 . decodeAs)
      , (195, singleton "IpDiffServCodePoint" . toJSON @Word8 . decodeAs)
      , (196, singleton "IpPrecedence" . toJSON @Word8 . decodeAs)
      , (197, singleton "FragmentFlags" . toJSON @Word8 . decodeAs)
      , (198, singleton "OctetDeltaSumOfSquares" . toJSON @Word64 . decodeAs)
      , (199, singleton "OctetTotalSumOfSquares" . toJSON @Word64 . decodeAs)
      , (200, singleton "MplsTopLabelTTL" . toJSON @Word8 . decodeAs)
      , (201, singleton "MplsLabelStackLength" . toJSON @Word32 . decodeAs)
      , (202, singleton "MplsLabelStackDepth" . toJSON @Word32 . decodeAs)
      , (203, singleton "MplsTopLabelExp" . toJSON @Word8 . decodeAs)
      , (204, singleton "IpPayloadLength" . toJSON @Word32 . decodeAs)
      , (205, singleton "UdpMessageLength" . toJSON @Word16 . decodeAs)
      , (206, singleton "IsMulticast" . toJSON @Word8 . decodeAs)
      , (207, singleton "Ipv4IHL" . toJSON @Word8 . decodeAs)
      , (208, singleton "Ipv4Options" . toJSON @Word32 . decodeAs)
      , (209, singleton "TcpOptions" . toJSON @Word64 . decodeAs)
      , (210, singleton "PaddingOctets" . toJSON @Bytes . decodeAs)
      , (211, singleton "CollectorIPv4Address" . toJSON @Addr4 . decodeAs)
      , (212, singleton "CollectorIPv6Address" . toJSON @Addr6 . decodeAs)
      , (213, singleton "ExportInterface" . toJSON @Word32 . decodeAs)
      , (214, singleton "ExportProtocolVersion" . toJSON @Word8 . decodeAs)
      , (215, singleton "ExportTransportProtocol" . toJSON @Word8 . decodeAs)
      , (216, singleton "CollectorTransportPort" . toJSON @Word16 . decodeAs)
      , (217, singleton "ExporterTransportPort" . toJSON @Word16 . decodeAs)
      , (218, singleton "TcpSynTotalCount" . toJSON @Word64 . decodeAs)
      , (219, singleton "TcpFinTotalCount" . toJSON @Word64 . decodeAs)
      , (220, singleton "TcpRstTotalCount" . toJSON @Word64 . decodeAs)
      , (221, singleton "TcpPshTotalCount" . toJSON @Word64 . decodeAs)
      , (222, singleton "TcpAckTotalCount" . toJSON @Word64 . decodeAs)
      , (223, singleton "TcpUrgTotalCount" . toJSON @Word64 . decodeAs)
      , (224, singleton "IpTotalLength" . toJSON @Word64 . decodeAs)
      , (225, singleton "PostNATSourceIPv4Address" . toJSON @Addr4 . decodeAs)
      , (226, singleton "PostNATDestinationIPv4Address" . toJSON @Addr4 . decodeAs)
      , (227, singleton "PostNAPTSourceTransportPort" . toJSON @Word16 . decodeAs)
      , (228, singleton "PostNAPTDestinationTransportPort" . toJSON @Word16 . decodeAs)
      , (229, singleton "NatOriginatingAddressRealm" . toJSON @Word8 . decodeAs)
      , (230, singleton "NatEvent" . toJSON @Word8 . decodeAs)
      , (231, singleton "InitiatorOctets" . toJSON @Word64 . decodeAs)
      , (232, singleton "ResponderOctets" . toJSON @Word64 . decodeAs)
      , (233, singleton "FirewallEvent" . toJSON @Word8 . decodeAs)
      , (234, singleton "IngressVRFID" . toJSON @Word32 . decodeAs)
      , (235, singleton "EgressVRFID" . toJSON @Word32 . decodeAs)
      , (236, singleton "VRFname" . toJSON @Text . decodeAs)
      , (237, singleton "PostMplsTopLabelExp" . toJSON @Word8 . decodeAs)
      , (238, singleton "TcpWindowScale" . toJSON @Word16 . decodeAs)
      , (239, singleton "BiflowDirection" . toJSON @Word8 . decodeAs)
      , (240, singleton "EthernetHeaderLength" . toJSON @Word8 . decodeAs)
      , (241, singleton "EthernetPayloadLength" . toJSON @Word16 . decodeAs)
      , (242, singleton "EthernetTotalLength" . toJSON @Word16 . decodeAs)
      , (243, singleton "Dot1qVlanId" . toJSON @Word16 . decodeAs)
      , (244, singleton "Dot1qPriority" . toJSON @Word8 . decodeAs)
      , (245, singleton "Dot1qCustomerVlanId" . toJSON @Word16 . decodeAs)
      , (246, singleton "Dot1qCustomerPriority" . toJSON @Word8 . decodeAs)
      , (247, singleton "MetroEvcId" . toJSON @Text . decodeAs)
      , (248, singleton "MetroEvcType" . toJSON @Word8 . decodeAs)
      , (249, singleton "PseudoWireId" . toJSON @Word32 . decodeAs)
      , (250, singleton "PseudoWireType" . toJSON @Word16 . decodeAs)
      , (251, singleton "PseudoWireControlWord" . toJSON @Word32 . decodeAs)
      , (252, singleton "IngressPhysicalInterface" . toJSON @Word32 . decodeAs)
      , (253, singleton "EgressPhysicalInterface" . toJSON @Word32 . decodeAs)
      , (254, singleton "PostDot1qVlanId" . toJSON @Word16 . decodeAs)
      , (255, singleton "PostDot1qCustomerVlanId" . toJSON @Word16 . decodeAs)
      , (256, singleton "EthernetType" . toJSON @Word16 . decodeAs)
      , (257, singleton "PostIpPrecedence" . toJSON @Word8 . decodeAs)
      , (258, singleton "CollectionTimeMilliseconds" . toJSON @Word . decodeAs)
      , (259, singleton "ExportSctpStreamId" . toJSON @Word16 . decodeAs)
      , (260, singleton "MaxExportSeconds" . toJSON @Word . decodeAs)
      , (261, singleton "MaxFlowEndSeconds" . toJSON @Word . decodeAs)
      , (262, singleton "MessageMD5Checksum" . toJSON @Bytes . decodeAs)
      , (263, singleton "MessageScope" . toJSON @Word8 . decodeAs)
      , (264, singleton "MinExportSeconds" . toJSON @Word . decodeAs)
      , (265, singleton "MinFlowStartSeconds" . toJSON @Word . decodeAs)
      , (266, singleton "OpaqueOctets" . toJSON @Bytes . decodeAs)
      , (267, singleton "SessionScope" . toJSON @Word8 . decodeAs)
      , (268, singleton "MaxFlowEndMicroseconds" . toJSON @Word . decodeAs)
      , (269, singleton "MaxFlowEndMilliseconds" . toJSON @Word . decodeAs)
      , (270, singleton "MaxFlowEndNanoseconds" . toJSON @Word . decodeAs)
      , (271, singleton "MinFlowStartMicroseconds" . toJSON @Word . decodeAs)
      , (272, singleton "MinFlowStartMilliseconds" . toJSON @Word . decodeAs)
      , (273, singleton "MinFlowStartNanoseconds" . toJSON @Word . decodeAs)
      , (274, singleton "CollectorCertificate" . toJSON @Bytes . decodeAs)
      , (275, singleton "ExporterCertificate" . toJSON @Bytes . decodeAs)
      , (276, singleton "DataRecordsReliability" . toJSON @Bool . decodeAs)
      , (277, singleton "ObservationPointType" . toJSON @Word8 . decodeAs)
      , (278, singleton "NewConnectionDeltaCount" . toJSON @Word32 . decodeAs)
      , (279, singleton "ConnectionSumDurationSeconds" . toJSON @Word64 . decodeAs)
      , (280, singleton "ConnectionTransactionId" . toJSON @Word64 . decodeAs)
      , (281, singleton "PostNATSourceIPv6Address" . toJSON @Addr6 . decodeAs)
      , (282, singleton "PostNATDestinationIPv6Address" . toJSON @Addr6 . decodeAs)
      , (283, singleton "NatPoolId" . toJSON @Word32 . decodeAs)
      , (284, singleton "NatPoolName" . toJSON @Text . decodeAs)
      , (285, singleton "AnonymizationFlags" . toJSON @Word16 . decodeAs)
      , (286, singleton "AnonymizationTechnique" . toJSON @Word16 . decodeAs)
      , (287, singleton "InformationElementIndex" . toJSON @Word16 . decodeAs)
      , (288, singleton "P2pTechnology" . toJSON @Text . decodeAs)
      , (289, singleton "TunnelTechnology" . toJSON @Text . decodeAs)
      , (290, singleton "EncryptedTechnology" . toJSON @Text . decodeAs)
      , (294, singleton "BgpValidityState" . toJSON @Word8 . decodeAs)
      , (295, singleton "IPSecSPI" . toJSON @Word32 . decodeAs)
      , (296, singleton "GreKey" . toJSON @Word32 . decodeAs)
      , (297, singleton "NatType" . toJSON @Word8 . decodeAs)
      , (298, singleton "InitiatorPackets" . toJSON @Word64 . decodeAs)
      , (299, singleton "ResponderPackets" . toJSON @Word64 . decodeAs)
      , (300, singleton "ObservationDomainName" . toJSON @Text . decodeAs)
      , (301, singleton "SelectionSequenceId" . toJSON @Word64 . decodeAs)
      , (302, singleton "SelectorId" . toJSON @Word64 . decodeAs)
      , (303, singleton "InformationElementId" . toJSON @Word16 . decodeAs)
      , (304, singleton "SelectorAlgorithm" . toJSON @Word16 . decodeAs)
      , (305, singleton "SamplingPacketInterval" . toJSON @Word32 . decodeAs)
      , (306, singleton "SamplingPacketSpace" . toJSON @Word32 . decodeAs)
      , (307, singleton "SamplingTimeInterval" . toJSON @Word32 . decodeAs)
      , (308, singleton "SamplingTimeSpace" . toJSON @Word32 . decodeAs)
      , (309, singleton "SamplingSize" . toJSON @Word32 . decodeAs)
      , (310, singleton "SamplingPopulation" . toJSON @Word32 . decodeAs)
      , (311, singleton "SamplingProbability" . toJSON @Double . decodeAs)
      , (312, singleton "DataLinkFrameSize" . toJSON @Word16 . decodeAs)
      , (313, singleton "IpHeaderPacketSection" . toJSON @Bytes . decodeAs)
      , (314, singleton "IpPayloadPacketSection" . toJSON @Bytes . decodeAs)
      , (315, singleton "DataLinkFrameSection" . toJSON @Bytes . decodeAs)
      , (316, singleton "MplsLabelStackSection" . toJSON @Bytes . decodeAs)
      , (317, singleton "MplsPayloadPacketSection" . toJSON @Bytes . decodeAs)
      , (318, singleton "SelectorIdTotalPktsObserved" . toJSON @Word64 . decodeAs)
      , (319, singleton "SelectorIdTotalPktsSelected" . toJSON @Word64 . decodeAs)
      , (320, singleton "AbsoluteError" . toJSON @Double . decodeAs)
      , (321, singleton "RelativeError" . toJSON @Double . decodeAs)
      , (322, singleton "ObservationTimeSeconds" . toJSON @Word . decodeAs)
      , (323, singleton "ObservationTimeMilliseconds" . toJSON @Word . decodeAs)
      , (324, singleton "ObservationTimeMicroseconds" . toJSON @Word . decodeAs)
      , (325, singleton "ObservationTimeNanoseconds" . toJSON @Word . decodeAs)
      , (326, singleton "DigestHashValue" . toJSON @Word64 . decodeAs)
      , (327, singleton "HashIPPayloadOffset" . toJSON @Word64 . decodeAs)
      , (328, singleton "HashIPPayloadSize" . toJSON @Word64 . decodeAs)
      , (329, singleton "HashOutputRangeMin" . toJSON @Word64 . decodeAs)
      , (330, singleton "HashOutputRangeMax" . toJSON @Word64 . decodeAs)
      , (331, singleton "HashSelectedRangeMin" . toJSON @Word64 . decodeAs)
      , (332, singleton "HashSelectedRangeMax" . toJSON @Word64 . decodeAs)
      , (333, singleton "HashDigestOutput" . toJSON @Bool . decodeAs)
      , (334, singleton "HashInitialiserValue" . toJSON @Word64 . decodeAs)
      , (335, singleton "SelectorName" . toJSON @Text . decodeAs)
      , (336, singleton "UpperCILimit" . toJSON @Double . decodeAs)
      , (337, singleton "LowerCILimit" . toJSON @Double . decodeAs)
      , (338, singleton "ConfidenceLevel" . toJSON @Double . decodeAs)
      , (339, singleton "InformationElementDataType" . toJSON @Word8 . decodeAs)
      , (340, singleton "InformationElementDescription" . toJSON @Text . decodeAs)
      , (341, singleton "InformationElementName" . toJSON @Text . decodeAs)
      , (342, singleton "InformationElementRangeBegin" . toJSON @Word64 . decodeAs)
      , (343, singleton "InformationElementRangeEnd" . toJSON @Word64 . decodeAs)
      , (344, singleton "InformationElementSemantics" . toJSON @Word8 . decodeAs)
      , (345, singleton "InformationElementUnits" . toJSON @Word16 . decodeAs)
      , (346, singleton "PrivateEnterpriseNumber" . toJSON @Word32 . decodeAs)
      , (347, singleton "VirtualStationInterfaceId" . toJSON @Bytes . decodeAs)
      , (348, singleton "VirtualStationInterfaceName" . toJSON @Text . decodeAs)
      , (349, singleton "VirtualStationUUID" . toJSON @Bytes . decodeAs)
      , (350, singleton "VirtualStationName" . toJSON @Text . decodeAs)
      , (351, singleton "Layer2SegmentId" . toJSON @Word64 . decodeAs)
      , (352, singleton "Layer2OctetDeltaCount" . toJSON @Word64 . decodeAs)
      , (353, singleton "Layer2OctetTotalCount" . toJSON @Word64 . decodeAs)
      , (354, singleton "IngressUnicastPacketTotalCount" . toJSON @Word64 . decodeAs)
      , (355, singleton "IngressMulticastPacketTotalCount" . toJSON @Word64 . decodeAs)
      , (356, singleton "IngressBroadcastPacketTotalCount" . toJSON @Word64 . decodeAs)
      , (357, singleton "EgressUnicastPacketTotalCount" . toJSON @Word64 . decodeAs)
      , (358, singleton "EgressBroadcastPacketTotalCount" . toJSON @Word64 . decodeAs)
      , (359, singleton "MonitoringIntervalStartMilliSeconds" . toJSON @Word . decodeAs)
      , (360, singleton "MonitoringIntervalEndMilliSeconds" . toJSON @Word . decodeAs)
      , (361, singleton "PortRangeStart" . toJSON @Word16 . decodeAs)
      , (362, singleton "PortRangeEnd" . toJSON @Word16 . decodeAs)
      , (363, singleton "PortRangeStepSize" . toJSON @Word16 . decodeAs)
      , (364, singleton "PortRangeNumPorts" . toJSON @Word16 . decodeAs)
      , (365, singleton "StaMacAddress" . toJSON @MAC . decodeAs)
      , (366, singleton "StaIPv4Address" . toJSON @Addr4 . decodeAs)
      , (367, singleton "WtpMacAddress" . toJSON @MAC . decodeAs)
      , (368, singleton "IngressInterfaceType" . toJSON @Word32 . decodeAs)
      , (369, singleton "EgressInterfaceType" . toJSON @Word32 . decodeAs)
      , (370, singleton "RtpSequenceNumber" . toJSON @Word16 . decodeAs)
      , (371, singleton "UserName" . toJSON @Text . decodeAs)
      , (372, singleton "ApplicationCategoryName" . toJSON @Text . decodeAs)
      , (373, singleton "ApplicationSubCategoryName" . toJSON @Text . decodeAs)
      , (374, singleton "ApplicationGroupName" . toJSON @Text . decodeAs)
      , (375, singleton "OriginalFlowsPresent" . toJSON @Word64 . decodeAs)
      , (376, singleton "OriginalFlowsInitiated" . toJSON @Word64 . decodeAs)
      , (377, singleton "OriginalFlowsCompleted" . toJSON @Word64 . decodeAs)
      , (378, singleton "DistinctCountOfSourceIPAddress" . toJSON @Word64 . decodeAs)
      , (379, singleton "DistinctCountOfDestinationIPAddress" . toJSON @Word64 . decodeAs)
      , (380, singleton "DistinctCountOfSourceIPv4Address" . toJSON @Word32 . decodeAs)
      , (381, singleton "DistinctCountOfDestinationIPv4Address" . toJSON @Word32 . decodeAs)
      , (382, singleton "DistinctCountOfSourceIPv6Address" . toJSON @Word64 . decodeAs)
      , (383, singleton "DistinctCountOfDestinationIPv6Address" . toJSON @Word64 . decodeAs)
      , (384, singleton "ValueDistributionMethod" . toJSON @Word8 . decodeAs)
      , (385, singleton "Rfc3550JitterMilliseconds" . toJSON @Word32 . decodeAs)
      , (386, singleton "Rfc3550JitterMicroseconds" . toJSON @Word32 . decodeAs)
      , (387, singleton "Rfc3550JitterNanoseconds" . toJSON @Word32 . decodeAs)
      , (388, singleton "Dot1qDEI" . toJSON @Bool . decodeAs)
      , (389, singleton "Dot1qCustomerDEI" . toJSON @Bool . decodeAs)
      , (390, singleton "FlowSelectorAlgorithm" . toJSON @Word16 . decodeAs)
      , (391, singleton "FlowSelectedOctetDeltaCount" . toJSON @Word64 . decodeAs)
      , (392, singleton "FlowSelectedPacketDeltaCount" . toJSON @Word64 . decodeAs)
      , (393, singleton "FlowSelectedFlowDeltaCount" . toJSON @Word64 . decodeAs)
      , (394, singleton "SelectorIDTotalFlowsObserved" . toJSON @Word64 . decodeAs)
      , (395, singleton "SelectorIDTotalFlowsSelected" . toJSON @Word64 . decodeAs)
      , (396, singleton "SamplingFlowInterval" . toJSON @Word64 . decodeAs)
      , (397, singleton "SamplingFlowSpacing" . toJSON @Word64 . decodeAs)
      , (398, singleton "FlowSamplingTimeInterval" . toJSON @Word64 . decodeAs)
      , (399, singleton "FlowSamplingTimeSpacing" . toJSON @Word64 . decodeAs)
      , (400, singleton "HashFlowDomain" . toJSON @Word16 . decodeAs)
      , (401, singleton "TransportOctetDeltaCount" . toJSON @Word64 . decodeAs)
      , (402, singleton "TransportPacketDeltaCount" . toJSON @Word64 . decodeAs)
      , (403, singleton "OriginalExporterIPv4Address" . toJSON @Addr4 . decodeAs)
      , (404, singleton "OriginalExporterIPv6Address" . toJSON @Addr6 . decodeAs)
      , (405, singleton "OriginalObservationDomainId" . toJSON @Word32 . decodeAs)
      , (406, singleton "IntermediateProcessId" . toJSON @Word32 . decodeAs)
      , (407, singleton "IgnoredDataRecordTotalCount" . toJSON @Word64 . decodeAs)
      , (408, singleton "DataLinkFrameType" . toJSON @Word16 . decodeAs)
      , (409, singleton "SectionOffset" . toJSON @Word16 . decodeAs)
      , (410, singleton "SectionExportedOctets" . toJSON @Word16 . decodeAs)
      , (411, singleton "Dot1qServiceInstanceTag" . toJSON @Bytes . decodeAs)
      , (412, singleton "Dot1qServiceInstanceId" . toJSON @Word32 . decodeAs)
      , (413, singleton "Dot1qServiceInstancePriority" . toJSON @Word8 . decodeAs)
      , (414, singleton "Dot1qCustomerSourceMacAddress" . toJSON @MAC . decodeAs)
      , (415, singleton "Dot1qCustomerDestinationMacAddress" . toJSON @MAC . decodeAs)
      , (417, singleton "PostLayer2OctetDeltaCount" . toJSON @Word64 . decodeAs)
      , (418, singleton "PostMCastLayer2OctetDeltaCount" . toJSON @Word64 . decodeAs)
      , (420, singleton "PostLayer2OctetTotalCount" . toJSON @Word64 . decodeAs)
      , (421, singleton "PostMCastLayer2OctetTotalCount" . toJSON @Word64 . decodeAs)
      , (422, singleton "MinimumLayer2TotalLength" . toJSON @Word64 . decodeAs)
      , (423, singleton "MaximumLayer2TotalLength" . toJSON @Word64 . decodeAs)
      , (424, singleton "DroppedLayer2OctetDeltaCount" . toJSON @Word64 . decodeAs)
      , (425, singleton "DroppedLayer2OctetTotalCount" . toJSON @Word64 . decodeAs)
      , (426, singleton "IgnoredLayer2OctetTotalCount" . toJSON @Word64 . decodeAs)
      , (427, singleton "NotSentLayer2OctetTotalCount" . toJSON @Word64 . decodeAs)
      , (428, singleton "Layer2OctetDeltaSumOfSquares" . toJSON @Word64 . decodeAs)
      , (429, singleton "Layer2OctetTotalSumOfSquares" . toJSON @Word64 . decodeAs)
      , (430, singleton "Layer2FrameDeltaCount" . toJSON @Word64 . decodeAs)
      , (431, singleton "Layer2FrameTotalCount" . toJSON @Word64 . decodeAs)
      , (432, singleton "PseudoWireDestinationIPv4Address" . toJSON @Addr4 . decodeAs)
      , (433, singleton "IgnoredLayer2FrameTotalCount" . toJSON @Word64 . decodeAs)
      , (434, singleton "MibObjectValueInteger" . toJSON @Int64 . decodeAs)
      , (435, singleton "MibObjectValueOctetString" . toJSON @Bytes . decodeAs)
      , (436, singleton "MibObjectValueOID" . toJSON @Bytes . decodeAs)
      , (437, singleton "MibObjectValueBits" . toJSON @Bytes . decodeAs)
      , (438, singleton "MibObjectValueIPAddress" . toJSON @Addr4 . decodeAs)
      , (439, singleton "MibObjectValueCounter" . toJSON @Word64 . decodeAs)
      , (440, singleton "MibObjectValueGauge" . toJSON @Word32 . decodeAs)
      , (441, singleton "MibObjectValueTimeTicks" . toJSON @Word32 . decodeAs)
      , (442, singleton "MibObjectValueUnsigned" . toJSON @Word64 . decodeAs)
      , (445, singleton "MibObjectIdentifier" . toJSON @Bytes . decodeAs)
      , (446, singleton "MibSubIdentifier" . toJSON @Word32 . decodeAs)
      , (447, singleton "MibIndexIndicator" . toJSON @Word64 . decodeAs)
      , (448, singleton "MibCaptureTimeSemantics" . toJSON @Word8 . decodeAs)
      , (449, singleton "MibContextEngineID" . toJSON @Bytes . decodeAs)
      , (450, singleton "MibContextName" . toJSON @Text . decodeAs)
      , (451, singleton "MibObjectName" . toJSON @Text . decodeAs)
      , (452, singleton "MibObjectDescription" . toJSON @Text . decodeAs)
      , (453, singleton "MibObjectSyntax" . toJSON @Text . decodeAs)
      , (454, singleton "MibModuleName" . toJSON @Text . decodeAs)
      , (455, singleton "MobileIMSI" . toJSON @Text . decodeAs)
      , (456, singleton "MobileMSISDN" . toJSON @Text . decodeAs)
      , (457, singleton "HttpStatusCode" . toJSON @Word16 . decodeAs)
      ]


-- vim:set ft=haskell sw=2 ts=2 et:
