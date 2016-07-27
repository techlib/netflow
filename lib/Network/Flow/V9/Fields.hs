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

module Network.Flow.V9.Fields
( Field(..)
, decodeField
) where
  import BasePrelude hiding (union, empty)

  import Data.MAC (MAC, toMAC)
  import Data.IP (IPv4, IPv6, toIPv4, toIPv6b)
  import Data.Text.Encoding (decodeUtf8, decodeUtf8')
  import Data.Serialize.Get
  import Data.Serialize.IEEE754

  import Data.HashMap.Strict (HashMap, union, empty, singleton)
  import Data.Text (Text)
  import Data.Aeson

  import qualified Data.ByteString.Base64 as Base64
  import qualified Data.ByteString as BS
  import qualified Data.Text as T

  data Field
    = OctetDeltaCount Word64
    | PacketDeltaCount Word64
    | DeltaFlowCount Word64
    | ProtocolIdentifier Word8
    | IpClassOfService Word8
    | TcpControlBits Word16
    | SourceTransportPort Word16
    | SourceIPv4Address IPv4
    | SourceIPv4PrefixLength Word8
    | IngressInterface Word32
    | DestinationTransportPort Word16
    | DestinationIPv4Address IPv4
    | DestinationIPv4PrefixLength Word8
    | EgressInterface Word32
    | IpNextHopIPv4Address IPv4
    | BgpSourceAsNumber Word32
    | BgpDestinationAsNumber Word32
    | BgpNextHopIPv4Address IPv4
    | PostMCastPacketDeltaCount Word64
    | PostMCastOctetDeltaCount Word64
    | FlowEndSysUpTime Word32
    | FlowStartSysUpTime Word32
    | PostOctetDeltaCount Word64
    | PostPacketDeltaCount Word64
    | MinimumIpTotalLength Word64
    | MaximumIpTotalLength Word64
    | SourceIPv6Address IPv6
    | DestinationIPv6Address IPv6
    | SourceIPv6PrefixLength Word8
    | DestinationIPv6PrefixLength Word8
    | FlowLabelIPv6 Word32
    | IcmpTypeCodeIPv4 Word16
    | IgmpType Word8
    | SamplingInterval Word32
    | SamplingAlgorithm Word8
    | FlowActiveTimeout Word16
    | FlowIdleTimeout Word16
    | EngineType Word8
    | EngineId Word8
    | ExportedOctetTotalCount Word64
    | ExportedMessageTotalCount Word64
    | ExportedFlowRecordTotalCount Word64
    | Ipv4RouterSc IPv4
    | SourceIPv4Prefix IPv4
    | DestinationIPv4Prefix IPv4
    | MplsTopLabelType Word8
    | MplsTopLabelIPv4Address IPv4
    | SamplerId Word8
    | SamplerMode Word8
    | SamplerRandomInterval Word32
    | ClassId Word8
    | MinimumTTL Word8
    | MaximumTTL Word8
    | FragmentIdentification Word32
    | PostIpClassOfService Word8
    | SourceMacAddress MAC
    | PostDestinationMacAddress MAC
    | VlanId Word16
    | PostVlanId Word16
    | IpVersion Word8
    | FlowDirection Word8
    | IpNextHopIPv6Address IPv6
    | BgpNextHopIPv6Address IPv6
    | Ipv6ExtensionHeaders Word32
    | MplsTopLabelStackSection BS.ByteString
    | MplsLabelStackSection2 BS.ByteString
    | MplsLabelStackSection3 BS.ByteString
    | MplsLabelStackSection4 BS.ByteString
    | MplsLabelStackSection5 BS.ByteString
    | MplsLabelStackSection6 BS.ByteString
    | MplsLabelStackSection7 BS.ByteString
    | MplsLabelStackSection8 BS.ByteString
    | MplsLabelStackSection9 BS.ByteString
    | MplsLabelStackSection10 BS.ByteString
    | DestinationMacAddress MAC
    | PostSourceMacAddress MAC
    | InterfaceName T.Text
    | InterfaceDescription T.Text
    | SamplerName T.Text
    | OctetTotalCount Word64
    | PacketTotalCount Word64
    | FlagsAndSamplerId Word32
    | FragmentOffset Word16
    | ForwardingStatus Word32
    | MplsVpnRouteDistinguisher BS.ByteString
    | MplsTopLabelPrefixLength Word8
    | SrcTrafficIndex Word32
    | DstTrafficIndex Word32
    | ApplicationDescription T.Text
    | ApplicationId BS.ByteString
    | ApplicationName T.Text
    | PostIpDiffServCodePoint Word8
    | MulticastReplicationFactor Word32
    | ClassName T.Text
    | ClassificationEngineId Word8
    | Layer2packetSectionOffset Word16
    | Layer2packetSectionSize Word16
    | Layer2packetSectionData BS.ByteString
    | BgpNextAdjacentAsNumber Word32
    | BgpPrevAdjacentAsNumber Word32
    | ExporterIPv4Address IPv4
    | ExporterIPv6Address IPv6
    | DroppedOctetDeltaCount Word64
    | DroppedPacketDeltaCount Word64
    | DroppedOctetTotalCount Word64
    | DroppedPacketTotalCount Word64
    | FlowEndReason Word8
    | CommonPropertiesId Word64
    | ObservationPointId Word64
    | IcmpTypeCodeIPv6 Word16
    | MplsTopLabelIPv6Address IPv6
    | LineCardId Word32
    | PortId Word32
    | MeteringProcessId Word32
    | ExportingProcessId Word32
    | TemplateId Word16
    | WlanChannelId Word8
    | WlanSSID T.Text
    | FlowId Word64
    | ObservationDomainId Word32
    | FlowStartSeconds Word
    | FlowEndSeconds Word
    | FlowStartMilliseconds Word
    | FlowEndMilliseconds Word
    | FlowStartMicroseconds Word
    | FlowEndMicroseconds Word
    | FlowStartNanoseconds Word
    | FlowEndNanoseconds Word
    | FlowStartDeltaMicroseconds Word32
    | FlowEndDeltaMicroseconds Word32
    | SystemInitTimeMilliseconds Word
    | FlowDurationMilliseconds Word32
    | FlowDurationMicroseconds Word32
    | ObservedFlowTotalCount Word64
    | IgnoredPacketTotalCount Word64
    | IgnoredOctetTotalCount Word64
    | NotSentFlowTotalCount Word64
    | NotSentPacketTotalCount Word64
    | NotSentOctetTotalCount Word64
    | DestinationIPv6Prefix IPv6
    | SourceIPv6Prefix IPv6
    | PostOctetTotalCount Word64
    | PostPacketTotalCount Word64
    | FlowKeyIndicator Word64
    | PostMCastPacketTotalCount Word64
    | PostMCastOctetTotalCount Word64
    | IcmpTypeIPv4 Word8
    | IcmpCodeIPv4 Word8
    | IcmpTypeIPv6 Word8
    | IcmpCodeIPv6 Word8
    | UdpSourcePort Word16
    | UdpDestinationPort Word16
    | TcpSourcePort Word16
    | TcpDestinationPort Word16
    | TcpSequenceNumber Word32
    | TcpAcknowledgementNumber Word32
    | TcpWindowSize Word16
    | TcpUrgentPointer Word16
    | TcpHeaderLength Word8
    | IpHeaderLength Word8
    | TotalLengthIPv4 Word16
    | PayloadLengthIPv6 Word16
    | IpTTL Word8
    | NextHeaderIPv6 Word8
    | MplsPayloadLength Word32
    | IpDiffServCodePoint Word8
    | IpPrecedence Word8
    | FragmentFlags Word8
    | OctetDeltaSumOfSquares Word64
    | OctetTotalSumOfSquares Word64
    | MplsTopLabelTTL Word8
    | MplsLabelStackLength Word32
    | MplsLabelStackDepth Word32
    | MplsTopLabelExp Word8
    | IpPayloadLength Word32
    | UdpMessageLength Word16
    | IsMulticast Word8
    | Ipv4IHL Word8
    | Ipv4Options Word32
    | TcpOptions Word64
    | PaddingOctets BS.ByteString
    | CollectorIPv4Address IPv4
    | CollectorIPv6Address IPv6
    | ExportInterface Word32
    | ExportProtocolVersion Word8
    | ExportTransportProtocol Word8
    | CollectorTransportPort Word16
    | ExporterTransportPort Word16
    | TcpSynTotalCount Word64
    | TcpFinTotalCount Word64
    | TcpRstTotalCount Word64
    | TcpPshTotalCount Word64
    | TcpAckTotalCount Word64
    | TcpUrgTotalCount Word64
    | IpTotalLength Word64
    | PostNATSourceIPv4Address IPv4
    | PostNATDestinationIPv4Address IPv4
    | PostNAPTSourceTransportPort Word16
    | PostNAPTDestinationTransportPort Word16
    | NatOriginatingAddressRealm Word8
    | NatEvent Word8
    | InitiatorOctets Word64
    | ResponderOctets Word64
    | FirewallEvent Word8
    | IngressVRFID Word32
    | EgressVRFID Word32
    | VRFname T.Text
    | PostMplsTopLabelExp Word8
    | TcpWindowScale Word16
    | BiflowDirection Word8
    | EthernetHeaderLength Word8
    | EthernetPayloadLength Word16
    | EthernetTotalLength Word16
    | Dot1qVlanId Word16
    | Dot1qPriority Word8
    | Dot1qCustomerVlanId Word16
    | Dot1qCustomerPriority Word8
    | MetroEvcId T.Text
    | MetroEvcType Word8
    | PseudoWireId Word32
    | PseudoWireType Word16
    | PseudoWireControlWord Word32
    | IngressPhysicalInterface Word32
    | EgressPhysicalInterface Word32
    | PostDot1qVlanId Word16
    | PostDot1qCustomerVlanId Word16
    | EthernetType Word16
    | PostIpPrecedence Word8
    | CollectionTimeMilliseconds Word
    | ExportSctpStreamId Word16
    | MaxExportSeconds Word
    | MaxFlowEndSeconds Word
    | MessageMD5Checksum BS.ByteString
    | MessageScope Word8
    | MinExportSeconds Word
    | MinFlowStartSeconds Word
    | OpaqueOctets BS.ByteString
    | SessionScope Word8
    | MaxFlowEndMicroseconds Word
    | MaxFlowEndMilliseconds Word
    | MaxFlowEndNanoseconds Word
    | MinFlowStartMicroseconds Word
    | MinFlowStartMilliseconds Word
    | MinFlowStartNanoseconds Word
    | CollectorCertificate BS.ByteString
    | ExporterCertificate BS.ByteString
    | DataRecordsReliability Bool
    | ObservationPointType Word8
    | NewConnectionDeltaCount Word32
    | ConnectionSumDurationSeconds Word64
    | ConnectionTransactionId Word64
    | PostNATSourceIPv6Address IPv6
    | PostNATDestinationIPv6Address IPv6
    | NatPoolId Word32
    | NatPoolName T.Text
    | AnonymizationFlags Word16
    | AnonymizationTechnique Word16
    | InformationElementIndex Word16
    | P2pTechnology T.Text
    | TunnelTechnology T.Text
    | EncryptedTechnology T.Text
    | BgpValidityState Word8
    | IPSecSPI Word32
    | GreKey Word32
    | NatType Word8
    | InitiatorPackets Word64
    | ResponderPackets Word64
    | ObservationDomainName T.Text
    | SelectionSequenceId Word64
    | SelectorId Word64
    | InformationElementId Word16
    | SelectorAlgorithm Word16
    | SamplingPacketInterval Word32
    | SamplingPacketSpace Word32
    | SamplingTimeInterval Word32
    | SamplingTimeSpace Word32
    | SamplingSize Word32
    | SamplingPopulation Word32
    | SamplingProbability Double
    | DataLinkFrameSize Word16
    | IpHeaderPacketSection BS.ByteString
    | IpPayloadPacketSection BS.ByteString
    | DataLinkFrameSection BS.ByteString
    | MplsLabelStackSection BS.ByteString
    | MplsPayloadPacketSection BS.ByteString
    | SelectorIdTotalPktsObserved Word64
    | SelectorIdTotalPktsSelected Word64
    | AbsoluteError Double
    | RelativeError Double
    | ObservationTimeSeconds Word
    | ObservationTimeMilliseconds Word
    | ObservationTimeMicroseconds Word
    | ObservationTimeNanoseconds Word
    | DigestHashValue Word64
    | HashIPPayloadOffset Word64
    | HashIPPayloadSize Word64
    | HashOutputRangeMin Word64
    | HashOutputRangeMax Word64
    | HashSelectedRangeMin Word64
    | HashSelectedRangeMax Word64
    | HashDigestOutput Bool
    | HashInitialiserValue Word64
    | SelectorName T.Text
    | UpperCILimit Double
    | LowerCILimit Double
    | ConfidenceLevel Double
    | InformationElementDataType Word8
    | InformationElementDescription T.Text
    | InformationElementName T.Text
    | InformationElementRangeBegin Word64
    | InformationElementRangeEnd Word64
    | InformationElementSemantics Word8
    | InformationElementUnits Word16
    | PrivateEnterpriseNumber Word32
    | VirtualStationInterfaceId BS.ByteString
    | VirtualStationInterfaceName T.Text
    | VirtualStationUUID BS.ByteString
    | VirtualStationName T.Text
    | Layer2SegmentId Word64
    | Layer2OctetDeltaCount Word64
    | Layer2OctetTotalCount Word64
    | IngressUnicastPacketTotalCount Word64
    | IngressMulticastPacketTotalCount Word64
    | IngressBroadcastPacketTotalCount Word64
    | EgressUnicastPacketTotalCount Word64
    | EgressBroadcastPacketTotalCount Word64
    | MonitoringIntervalStartMilliSeconds Word
    | MonitoringIntervalEndMilliSeconds Word
    | PortRangeStart Word16
    | PortRangeEnd Word16
    | PortRangeStepSize Word16
    | PortRangeNumPorts Word16
    | StaMacAddress MAC
    | StaIPv4Address IPv4
    | WtpMacAddress MAC
    | IngressInterfaceType Word32
    | EgressInterfaceType Word32
    | RtpSequenceNumber Word16
    | UserName T.Text
    | ApplicationCategoryName T.Text
    | ApplicationSubCategoryName T.Text
    | ApplicationGroupName T.Text
    | OriginalFlowsPresent Word64
    | OriginalFlowsInitiated Word64
    | OriginalFlowsCompleted Word64
    | DistinctCountOfSourceIPAddress Word64
    | DistinctCountOfDestinationIPAddress Word64
    | DistinctCountOfSourceIPv4Address Word32
    | DistinctCountOfDestinationIPv4Address Word32
    | DistinctCountOfSourceIPv6Address Word64
    | DistinctCountOfDestinationIPv6Address Word64
    | ValueDistributionMethod Word8
    | Rfc3550JitterMilliseconds Word32
    | Rfc3550JitterMicroseconds Word32
    | Rfc3550JitterNanoseconds Word32
    | Dot1qDEI Bool
    | Dot1qCustomerDEI Bool
    | FlowSelectorAlgorithm Word16
    | FlowSelectedOctetDeltaCount Word64
    | FlowSelectedPacketDeltaCount Word64
    | FlowSelectedFlowDeltaCount Word64
    | SelectorIDTotalFlowsObserved Word64
    | SelectorIDTotalFlowsSelected Word64
    | SamplingFlowInterval Word64
    | SamplingFlowSpacing Word64
    | FlowSamplingTimeInterval Word64
    | FlowSamplingTimeSpacing Word64
    | HashFlowDomain Word16
    | TransportOctetDeltaCount Word64
    | TransportPacketDeltaCount Word64
    | OriginalExporterIPv4Address IPv4
    | OriginalExporterIPv6Address IPv6
    | OriginalObservationDomainId Word32
    | IntermediateProcessId Word32
    | IgnoredDataRecordTotalCount Word64
    | DataLinkFrameType Word16
    | SectionOffset Word16
    | SectionExportedOctets Word16
    | Dot1qServiceInstanceTag BS.ByteString
    | Dot1qServiceInstanceId Word32
    | Dot1qServiceInstancePriority Word8
    | Dot1qCustomerSourceMacAddress MAC
    | Dot1qCustomerDestinationMacAddress MAC
    | PostLayer2OctetDeltaCount Word64
    | PostMCastLayer2OctetDeltaCount Word64
    | PostLayer2OctetTotalCount Word64
    | PostMCastLayer2OctetTotalCount Word64
    | MinimumLayer2TotalLength Word64
    | MaximumLayer2TotalLength Word64
    | DroppedLayer2OctetDeltaCount Word64
    | DroppedLayer2OctetTotalCount Word64
    | IgnoredLayer2OctetTotalCount Word64
    | NotSentLayer2OctetTotalCount Word64
    | Layer2OctetDeltaSumOfSquares Word64
    | Layer2OctetTotalSumOfSquares Word64
    | Layer2FrameDeltaCount Word64
    | Layer2FrameTotalCount Word64
    | PseudoWireDestinationIPv4Address IPv4
    | IgnoredLayer2FrameTotalCount Word64
    | MibObjectValueInteger Int64
    | MibObjectValueOctetString BS.ByteString
    | MibObjectValueOID BS.ByteString
    | MibObjectValueBits BS.ByteString
    | MibObjectValueIPAddress IPv4
    | MibObjectValueCounter Word64
    | MibObjectValueGauge Word32
    | MibObjectValueTimeTicks Word32
    | MibObjectValueUnsigned Word64
    | MibObjectIdentifier BS.ByteString
    | MibSubIdentifier Word32
    | MibIndexIndicator Word64
    | MibCaptureTimeSemantics Word8
    | MibContextEngineID BS.ByteString
    | MibContextName T.Text
    | MibObjectName T.Text
    | MibObjectDescription T.Text
    | MibObjectSyntax T.Text
    | MibModuleName T.Text
    | MobileIMSI T.Text
    | MobileMSISDN T.Text
    | HttpStatusCode Word16
    | OtherField Word16 BS.ByteString
    deriving (Show, Eq)


  decodeField :: Word16 -> BS.ByteString -> Field
  decodeField   1 = decodeAs   1 OctetDeltaCount
  decodeField   2 = decodeAs   2 PacketDeltaCount
  decodeField   3 = decodeAs   3 DeltaFlowCount
  decodeField   4 = decodeAs   4 ProtocolIdentifier
  decodeField   5 = decodeAs   5 IpClassOfService
  decodeField   6 = decodeAs   6 TcpControlBits
  decodeField   7 = decodeAs   7 SourceTransportPort
  decodeField   8 = decodeAs   8 SourceIPv4Address
  decodeField   9 = decodeAs   9 SourceIPv4PrefixLength
  decodeField  10 = decodeAs  10 IngressInterface
  decodeField  11 = decodeAs  11 DestinationTransportPort
  decodeField  12 = decodeAs  12 DestinationIPv4Address
  decodeField  13 = decodeAs  13 DestinationIPv4PrefixLength
  decodeField  14 = decodeAs  14 EgressInterface
  decodeField  15 = decodeAs  15 IpNextHopIPv4Address
  decodeField  16 = decodeAs  16 BgpSourceAsNumber
  decodeField  17 = decodeAs  17 BgpDestinationAsNumber
  decodeField  18 = decodeAs  18 BgpNextHopIPv4Address
  decodeField  19 = decodeAs  19 PostMCastPacketDeltaCount
  decodeField  20 = decodeAs  20 PostMCastOctetDeltaCount
  decodeField  21 = decodeAs  21 FlowEndSysUpTime
  decodeField  22 = decodeAs  22 FlowStartSysUpTime
  decodeField  23 = decodeAs  23 PostOctetDeltaCount
  decodeField  24 = decodeAs  24 PostPacketDeltaCount
  decodeField  25 = decodeAs  25 MinimumIpTotalLength
  decodeField  26 = decodeAs  26 MaximumIpTotalLength
  decodeField  27 = decodeAs  27 SourceIPv6Address
  decodeField  28 = decodeAs  28 DestinationIPv6Address
  decodeField  29 = decodeAs  29 SourceIPv6PrefixLength
  decodeField  30 = decodeAs  30 DestinationIPv6PrefixLength
  decodeField  31 = decodeAs  31 FlowLabelIPv6
  decodeField  32 = decodeAs  32 IcmpTypeCodeIPv4
  decodeField  33 = decodeAs  33 IgmpType
  decodeField  34 = decodeAs  34 SamplingInterval
  decodeField  35 = decodeAs  35 SamplingAlgorithm
  decodeField  36 = decodeAs  36 FlowActiveTimeout
  decodeField  37 = decodeAs  37 FlowIdleTimeout
  decodeField  38 = decodeAs  38 EngineType
  decodeField  39 = decodeAs  39 EngineId
  decodeField  40 = decodeAs  40 ExportedOctetTotalCount
  decodeField  41 = decodeAs  41 ExportedMessageTotalCount
  decodeField  42 = decodeAs  42 ExportedFlowRecordTotalCount
  decodeField  43 = decodeAs  43 Ipv4RouterSc
  decodeField  44 = decodeAs  44 SourceIPv4Prefix
  decodeField  45 = decodeAs  45 DestinationIPv4Prefix
  decodeField  46 = decodeAs  46 MplsTopLabelType
  decodeField  47 = decodeAs  47 MplsTopLabelIPv4Address
  decodeField  48 = decodeAs  48 SamplerId
  decodeField  49 = decodeAs  49 SamplerMode
  decodeField  50 = decodeAs  50 SamplerRandomInterval
  decodeField  51 = decodeAs  51 ClassId
  decodeField  52 = decodeAs  52 MinimumTTL
  decodeField  53 = decodeAs  53 MaximumTTL
  decodeField  54 = decodeAs  54 FragmentIdentification
  decodeField  55 = decodeAs  55 PostIpClassOfService
  decodeField  56 = decodeAs  56 SourceMacAddress
  decodeField  57 = decodeAs  57 PostDestinationMacAddress
  decodeField  58 = decodeAs  58 VlanId
  decodeField  59 = decodeAs  59 PostVlanId
  decodeField  60 = decodeAs  60 IpVersion
  decodeField  61 = decodeAs  61 FlowDirection
  decodeField  62 = decodeAs  62 IpNextHopIPv6Address
  decodeField  63 = decodeAs  63 BgpNextHopIPv6Address
  decodeField  64 = decodeAs  64 Ipv6ExtensionHeaders
  decodeField  70 = decodeAs  70 MplsTopLabelStackSection
  decodeField  71 = decodeAs  71 MplsLabelStackSection2
  decodeField  72 = decodeAs  72 MplsLabelStackSection3
  decodeField  73 = decodeAs  73 MplsLabelStackSection4
  decodeField  74 = decodeAs  74 MplsLabelStackSection5
  decodeField  75 = decodeAs  75 MplsLabelStackSection6
  decodeField  76 = decodeAs  76 MplsLabelStackSection7
  decodeField  77 = decodeAs  77 MplsLabelStackSection8
  decodeField  78 = decodeAs  78 MplsLabelStackSection9
  decodeField  79 = decodeAs  79 MplsLabelStackSection10
  decodeField  80 = decodeAs  80 DestinationMacAddress
  decodeField  81 = decodeAs  81 PostSourceMacAddress
  decodeField  82 = decodeAs  82 InterfaceName
  decodeField  83 = decodeAs  83 InterfaceDescription
  decodeField  84 = decodeAs  84 SamplerName
  decodeField  85 = decodeAs  85 OctetTotalCount
  decodeField  86 = decodeAs  86 PacketTotalCount
  decodeField  87 = decodeAs  87 FlagsAndSamplerId
  decodeField  88 = decodeAs  88 FragmentOffset
  decodeField  89 = decodeAs  89 ForwardingStatus
  decodeField  90 = decodeAs  90 MplsVpnRouteDistinguisher
  decodeField  91 = decodeAs  91 MplsTopLabelPrefixLength
  decodeField  92 = decodeAs  92 SrcTrafficIndex
  decodeField  93 = decodeAs  93 DstTrafficIndex
  decodeField  94 = decodeAs  94 ApplicationDescription
  decodeField  95 = decodeAs  95 ApplicationId
  decodeField  96 = decodeAs  96 ApplicationName
  decodeField  98 = decodeAs  98 PostIpDiffServCodePoint
  decodeField  99 = decodeAs  99 MulticastReplicationFactor
  decodeField 100 = decodeAs 100 ClassName
  decodeField 101 = decodeAs 101 ClassificationEngineId
  decodeField 102 = decodeAs 102 Layer2packetSectionOffset
  decodeField 103 = decodeAs 103 Layer2packetSectionSize
  decodeField 104 = decodeAs 104 Layer2packetSectionData
  decodeField 128 = decodeAs 128 BgpNextAdjacentAsNumber
  decodeField 129 = decodeAs 129 BgpPrevAdjacentAsNumber
  decodeField 130 = decodeAs 130 ExporterIPv4Address
  decodeField 131 = decodeAs 131 ExporterIPv6Address
  decodeField 132 = decodeAs 132 DroppedOctetDeltaCount
  decodeField 133 = decodeAs 133 DroppedPacketDeltaCount
  decodeField 134 = decodeAs 134 DroppedOctetTotalCount
  decodeField 135 = decodeAs 135 DroppedPacketTotalCount
  decodeField 136 = decodeAs 136 FlowEndReason
  decodeField 137 = decodeAs 137 CommonPropertiesId
  decodeField 138 = decodeAs 138 ObservationPointId
  decodeField 139 = decodeAs 139 IcmpTypeCodeIPv6
  decodeField 140 = decodeAs 140 MplsTopLabelIPv6Address
  decodeField 141 = decodeAs 141 LineCardId
  decodeField 142 = decodeAs 142 PortId
  decodeField 143 = decodeAs 143 MeteringProcessId
  decodeField 144 = decodeAs 144 ExportingProcessId
  decodeField 145 = decodeAs 145 TemplateId
  decodeField 146 = decodeAs 146 WlanChannelId
  decodeField 147 = decodeAs 147 WlanSSID
  decodeField 148 = decodeAs 148 FlowId
  decodeField 149 = decodeAs 149 ObservationDomainId
  decodeField 150 = decodeAs 150 FlowStartSeconds
  decodeField 151 = decodeAs 151 FlowEndSeconds
  decodeField 152 = decodeAs 152 FlowStartMilliseconds
  decodeField 153 = decodeAs 153 FlowEndMilliseconds
  decodeField 154 = decodeAs 154 FlowStartMicroseconds
  decodeField 155 = decodeAs 155 FlowEndMicroseconds
  decodeField 156 = decodeAs 156 FlowStartNanoseconds
  decodeField 157 = decodeAs 157 FlowEndNanoseconds
  decodeField 158 = decodeAs 158 FlowStartDeltaMicroseconds
  decodeField 159 = decodeAs 159 FlowEndDeltaMicroseconds
  decodeField 160 = decodeAs 160 SystemInitTimeMilliseconds
  decodeField 161 = decodeAs 161 FlowDurationMilliseconds
  decodeField 162 = decodeAs 162 FlowDurationMicroseconds
  decodeField 163 = decodeAs 163 ObservedFlowTotalCount
  decodeField 164 = decodeAs 164 IgnoredPacketTotalCount
  decodeField 165 = decodeAs 165 IgnoredOctetTotalCount
  decodeField 166 = decodeAs 166 NotSentFlowTotalCount
  decodeField 167 = decodeAs 167 NotSentPacketTotalCount
  decodeField 168 = decodeAs 168 NotSentOctetTotalCount
  decodeField 169 = decodeAs 169 DestinationIPv6Prefix
  decodeField 170 = decodeAs 170 SourceIPv6Prefix
  decodeField 171 = decodeAs 171 PostOctetTotalCount
  decodeField 172 = decodeAs 172 PostPacketTotalCount
  decodeField 173 = decodeAs 173 FlowKeyIndicator
  decodeField 174 = decodeAs 174 PostMCastPacketTotalCount
  decodeField 175 = decodeAs 175 PostMCastOctetTotalCount
  decodeField 176 = decodeAs 176 IcmpTypeIPv4
  decodeField 177 = decodeAs 177 IcmpCodeIPv4
  decodeField 178 = decodeAs 178 IcmpTypeIPv6
  decodeField 179 = decodeAs 179 IcmpCodeIPv6
  decodeField 180 = decodeAs 180 UdpSourcePort
  decodeField 181 = decodeAs 181 UdpDestinationPort
  decodeField 182 = decodeAs 182 TcpSourcePort
  decodeField 183 = decodeAs 183 TcpDestinationPort
  decodeField 184 = decodeAs 184 TcpSequenceNumber
  decodeField 185 = decodeAs 185 TcpAcknowledgementNumber
  decodeField 186 = decodeAs 186 TcpWindowSize
  decodeField 187 = decodeAs 187 TcpUrgentPointer
  decodeField 188 = decodeAs 188 TcpHeaderLength
  decodeField 189 = decodeAs 189 IpHeaderLength
  decodeField 190 = decodeAs 190 TotalLengthIPv4
  decodeField 191 = decodeAs 191 PayloadLengthIPv6
  decodeField 192 = decodeAs 192 IpTTL
  decodeField 193 = decodeAs 193 NextHeaderIPv6
  decodeField 194 = decodeAs 194 MplsPayloadLength
  decodeField 195 = decodeAs 195 IpDiffServCodePoint
  decodeField 196 = decodeAs 196 IpPrecedence
  decodeField 197 = decodeAs 197 FragmentFlags
  decodeField 198 = decodeAs 198 OctetDeltaSumOfSquares
  decodeField 199 = decodeAs 199 OctetTotalSumOfSquares
  decodeField 200 = decodeAs 200 MplsTopLabelTTL
  decodeField 201 = decodeAs 201 MplsLabelStackLength
  decodeField 202 = decodeAs 202 MplsLabelStackDepth
  decodeField 203 = decodeAs 203 MplsTopLabelExp
  decodeField 204 = decodeAs 204 IpPayloadLength
  decodeField 205 = decodeAs 205 UdpMessageLength
  decodeField 206 = decodeAs 206 IsMulticast
  decodeField 207 = decodeAs 207 Ipv4IHL
  decodeField 208 = decodeAs 208 Ipv4Options
  decodeField 209 = decodeAs 209 TcpOptions
  decodeField 210 = decodeAs 210 PaddingOctets
  decodeField 211 = decodeAs 211 CollectorIPv4Address
  decodeField 212 = decodeAs 212 CollectorIPv6Address
  decodeField 213 = decodeAs 213 ExportInterface
  decodeField 214 = decodeAs 214 ExportProtocolVersion
  decodeField 215 = decodeAs 215 ExportTransportProtocol
  decodeField 216 = decodeAs 216 CollectorTransportPort
  decodeField 217 = decodeAs 217 ExporterTransportPort
  decodeField 218 = decodeAs 218 TcpSynTotalCount
  decodeField 219 = decodeAs 219 TcpFinTotalCount
  decodeField 220 = decodeAs 220 TcpRstTotalCount
  decodeField 221 = decodeAs 221 TcpPshTotalCount
  decodeField 222 = decodeAs 222 TcpAckTotalCount
  decodeField 223 = decodeAs 223 TcpUrgTotalCount
  decodeField 224 = decodeAs 224 IpTotalLength
  decodeField 225 = decodeAs 225 PostNATSourceIPv4Address
  decodeField 226 = decodeAs 226 PostNATDestinationIPv4Address
  decodeField 227 = decodeAs 227 PostNAPTSourceTransportPort
  decodeField 228 = decodeAs 228 PostNAPTDestinationTransportPort
  decodeField 229 = decodeAs 229 NatOriginatingAddressRealm
  decodeField 230 = decodeAs 230 NatEvent
  decodeField 231 = decodeAs 231 InitiatorOctets
  decodeField 232 = decodeAs 232 ResponderOctets
  decodeField 233 = decodeAs 233 FirewallEvent
  decodeField 234 = decodeAs 234 IngressVRFID
  decodeField 235 = decodeAs 235 EgressVRFID
  decodeField 236 = decodeAs 236 VRFname
  decodeField 237 = decodeAs 237 PostMplsTopLabelExp
  decodeField 238 = decodeAs 238 TcpWindowScale
  decodeField 239 = decodeAs 239 BiflowDirection
  decodeField 240 = decodeAs 240 EthernetHeaderLength
  decodeField 241 = decodeAs 241 EthernetPayloadLength
  decodeField 242 = decodeAs 242 EthernetTotalLength
  decodeField 243 = decodeAs 243 Dot1qVlanId
  decodeField 244 = decodeAs 244 Dot1qPriority
  decodeField 245 = decodeAs 245 Dot1qCustomerVlanId
  decodeField 246 = decodeAs 246 Dot1qCustomerPriority
  decodeField 247 = decodeAs 247 MetroEvcId
  decodeField 248 = decodeAs 248 MetroEvcType
  decodeField 249 = decodeAs 249 PseudoWireId
  decodeField 250 = decodeAs 250 PseudoWireType
  decodeField 251 = decodeAs 251 PseudoWireControlWord
  decodeField 252 = decodeAs 252 IngressPhysicalInterface
  decodeField 253 = decodeAs 253 EgressPhysicalInterface
  decodeField 254 = decodeAs 254 PostDot1qVlanId
  decodeField 255 = decodeAs 255 PostDot1qCustomerVlanId
  decodeField 256 = decodeAs 256 EthernetType
  decodeField 257 = decodeAs 257 PostIpPrecedence
  decodeField 258 = decodeAs 258 CollectionTimeMilliseconds
  decodeField 259 = decodeAs 259 ExportSctpStreamId
  decodeField 260 = decodeAs 260 MaxExportSeconds
  decodeField 261 = decodeAs 261 MaxFlowEndSeconds
  decodeField 262 = decodeAs 262 MessageMD5Checksum
  decodeField 263 = decodeAs 263 MessageScope
  decodeField 264 = decodeAs 264 MinExportSeconds
  decodeField 265 = decodeAs 265 MinFlowStartSeconds
  decodeField 266 = decodeAs 266 OpaqueOctets
  decodeField 267 = decodeAs 267 SessionScope
  decodeField 268 = decodeAs 268 MaxFlowEndMicroseconds
  decodeField 269 = decodeAs 269 MaxFlowEndMilliseconds
  decodeField 270 = decodeAs 270 MaxFlowEndNanoseconds
  decodeField 271 = decodeAs 271 MinFlowStartMicroseconds
  decodeField 272 = decodeAs 272 MinFlowStartMilliseconds
  decodeField 273 = decodeAs 273 MinFlowStartNanoseconds
  decodeField 274 = decodeAs 274 CollectorCertificate
  decodeField 275 = decodeAs 275 ExporterCertificate
  decodeField 276 = decodeAs 276 DataRecordsReliability
  decodeField 277 = decodeAs 277 ObservationPointType
  decodeField 278 = decodeAs 278 NewConnectionDeltaCount
  decodeField 279 = decodeAs 279 ConnectionSumDurationSeconds
  decodeField 280 = decodeAs 280 ConnectionTransactionId
  decodeField 281 = decodeAs 281 PostNATSourceIPv6Address
  decodeField 282 = decodeAs 282 PostNATDestinationIPv6Address
  decodeField 283 = decodeAs 283 NatPoolId
  decodeField 284 = decodeAs 284 NatPoolName
  decodeField 285 = decodeAs 285 AnonymizationFlags
  decodeField 286 = decodeAs 286 AnonymizationTechnique
  decodeField 287 = decodeAs 287 InformationElementIndex
  decodeField 288 = decodeAs 288 P2pTechnology
  decodeField 289 = decodeAs 289 TunnelTechnology
  decodeField 290 = decodeAs 290 EncryptedTechnology
  decodeField 294 = decodeAs 294 BgpValidityState
  decodeField 295 = decodeAs 295 IPSecSPI
  decodeField 296 = decodeAs 296 GreKey
  decodeField 297 = decodeAs 297 NatType
  decodeField 298 = decodeAs 298 InitiatorPackets
  decodeField 299 = decodeAs 299 ResponderPackets
  decodeField 300 = decodeAs 300 ObservationDomainName
  decodeField 301 = decodeAs 301 SelectionSequenceId
  decodeField 302 = decodeAs 302 SelectorId
  decodeField 303 = decodeAs 303 InformationElementId
  decodeField 304 = decodeAs 304 SelectorAlgorithm
  decodeField 305 = decodeAs 305 SamplingPacketInterval
  decodeField 306 = decodeAs 306 SamplingPacketSpace
  decodeField 307 = decodeAs 307 SamplingTimeInterval
  decodeField 308 = decodeAs 308 SamplingTimeSpace
  decodeField 309 = decodeAs 309 SamplingSize
  decodeField 310 = decodeAs 310 SamplingPopulation
  decodeField 311 = decodeAs 311 SamplingProbability
  decodeField 312 = decodeAs 312 DataLinkFrameSize
  decodeField 313 = decodeAs 313 IpHeaderPacketSection
  decodeField 314 = decodeAs 314 IpPayloadPacketSection
  decodeField 315 = decodeAs 315 DataLinkFrameSection
  decodeField 316 = decodeAs 316 MplsLabelStackSection
  decodeField 317 = decodeAs 317 MplsPayloadPacketSection
  decodeField 318 = decodeAs 318 SelectorIdTotalPktsObserved
  decodeField 319 = decodeAs 319 SelectorIdTotalPktsSelected
  decodeField 320 = decodeAs 320 AbsoluteError
  decodeField 321 = decodeAs 321 RelativeError
  decodeField 322 = decodeAs 322 ObservationTimeSeconds
  decodeField 323 = decodeAs 323 ObservationTimeMilliseconds
  decodeField 324 = decodeAs 324 ObservationTimeMicroseconds
  decodeField 325 = decodeAs 325 ObservationTimeNanoseconds
  decodeField 326 = decodeAs 326 DigestHashValue
  decodeField 327 = decodeAs 327 HashIPPayloadOffset
  decodeField 328 = decodeAs 328 HashIPPayloadSize
  decodeField 329 = decodeAs 329 HashOutputRangeMin
  decodeField 330 = decodeAs 330 HashOutputRangeMax
  decodeField 331 = decodeAs 331 HashSelectedRangeMin
  decodeField 332 = decodeAs 332 HashSelectedRangeMax
  decodeField 333 = decodeAs 333 HashDigestOutput
  decodeField 334 = decodeAs 334 HashInitialiserValue
  decodeField 335 = decodeAs 335 SelectorName
  decodeField 336 = decodeAs 336 UpperCILimit
  decodeField 337 = decodeAs 337 LowerCILimit
  decodeField 338 = decodeAs 338 ConfidenceLevel
  decodeField 339 = decodeAs 339 InformationElementDataType
  decodeField 340 = decodeAs 340 InformationElementDescription
  decodeField 341 = decodeAs 341 InformationElementName
  decodeField 342 = decodeAs 342 InformationElementRangeBegin
  decodeField 343 = decodeAs 343 InformationElementRangeEnd
  decodeField 344 = decodeAs 344 InformationElementSemantics
  decodeField 345 = decodeAs 345 InformationElementUnits
  decodeField 346 = decodeAs 346 PrivateEnterpriseNumber
  decodeField 347 = decodeAs 347 VirtualStationInterfaceId
  decodeField 348 = decodeAs 348 VirtualStationInterfaceName
  decodeField 349 = decodeAs 349 VirtualStationUUID
  decodeField 350 = decodeAs 350 VirtualStationName
  decodeField 351 = decodeAs 351 Layer2SegmentId
  decodeField 352 = decodeAs 352 Layer2OctetDeltaCount
  decodeField 353 = decodeAs 353 Layer2OctetTotalCount
  decodeField 354 = decodeAs 354 IngressUnicastPacketTotalCount
  decodeField 355 = decodeAs 355 IngressMulticastPacketTotalCount
  decodeField 356 = decodeAs 356 IngressBroadcastPacketTotalCount
  decodeField 357 = decodeAs 357 EgressUnicastPacketTotalCount
  decodeField 358 = decodeAs 358 EgressBroadcastPacketTotalCount
  decodeField 359 = decodeAs 359 MonitoringIntervalStartMilliSeconds
  decodeField 360 = decodeAs 360 MonitoringIntervalEndMilliSeconds
  decodeField 361 = decodeAs 361 PortRangeStart
  decodeField 362 = decodeAs 362 PortRangeEnd
  decodeField 363 = decodeAs 363 PortRangeStepSize
  decodeField 364 = decodeAs 364 PortRangeNumPorts
  decodeField 365 = decodeAs 365 StaMacAddress
  decodeField 366 = decodeAs 366 StaIPv4Address
  decodeField 367 = decodeAs 367 WtpMacAddress
  decodeField 368 = decodeAs 368 IngressInterfaceType
  decodeField 369 = decodeAs 369 EgressInterfaceType
  decodeField 370 = decodeAs 370 RtpSequenceNumber
  decodeField 371 = decodeAs 371 UserName
  decodeField 372 = decodeAs 372 ApplicationCategoryName
  decodeField 373 = decodeAs 373 ApplicationSubCategoryName
  decodeField 374 = decodeAs 374 ApplicationGroupName
  decodeField 375 = decodeAs 375 OriginalFlowsPresent
  decodeField 376 = decodeAs 376 OriginalFlowsInitiated
  decodeField 377 = decodeAs 377 OriginalFlowsCompleted
  decodeField 378 = decodeAs 378 DistinctCountOfSourceIPAddress
  decodeField 379 = decodeAs 379 DistinctCountOfDestinationIPAddress
  decodeField 380 = decodeAs 380 DistinctCountOfSourceIPv4Address
  decodeField 381 = decodeAs 381 DistinctCountOfDestinationIPv4Address
  decodeField 382 = decodeAs 382 DistinctCountOfSourceIPv6Address
  decodeField 383 = decodeAs 383 DistinctCountOfDestinationIPv6Address
  decodeField 384 = decodeAs 384 ValueDistributionMethod
  decodeField 385 = decodeAs 385 Rfc3550JitterMilliseconds
  decodeField 386 = decodeAs 386 Rfc3550JitterMicroseconds
  decodeField 387 = decodeAs 387 Rfc3550JitterNanoseconds
  decodeField 388 = decodeAs 388 Dot1qDEI
  decodeField 389 = decodeAs 389 Dot1qCustomerDEI
  decodeField 390 = decodeAs 390 FlowSelectorAlgorithm
  decodeField 391 = decodeAs 391 FlowSelectedOctetDeltaCount
  decodeField 392 = decodeAs 392 FlowSelectedPacketDeltaCount
  decodeField 393 = decodeAs 393 FlowSelectedFlowDeltaCount
  decodeField 394 = decodeAs 394 SelectorIDTotalFlowsObserved
  decodeField 395 = decodeAs 395 SelectorIDTotalFlowsSelected
  decodeField 396 = decodeAs 396 SamplingFlowInterval
  decodeField 397 = decodeAs 397 SamplingFlowSpacing
  decodeField 398 = decodeAs 398 FlowSamplingTimeInterval
  decodeField 399 = decodeAs 399 FlowSamplingTimeSpacing
  decodeField 400 = decodeAs 400 HashFlowDomain
  decodeField 401 = decodeAs 401 TransportOctetDeltaCount
  decodeField 402 = decodeAs 402 TransportPacketDeltaCount
  decodeField 403 = decodeAs 403 OriginalExporterIPv4Address
  decodeField 404 = decodeAs 404 OriginalExporterIPv6Address
  decodeField 405 = decodeAs 405 OriginalObservationDomainId
  decodeField 406 = decodeAs 406 IntermediateProcessId
  decodeField 407 = decodeAs 407 IgnoredDataRecordTotalCount
  decodeField 408 = decodeAs 408 DataLinkFrameType
  decodeField 409 = decodeAs 409 SectionOffset
  decodeField 410 = decodeAs 410 SectionExportedOctets
  decodeField 411 = decodeAs 411 Dot1qServiceInstanceTag
  decodeField 412 = decodeAs 412 Dot1qServiceInstanceId
  decodeField 413 = decodeAs 413 Dot1qServiceInstancePriority
  decodeField 414 = decodeAs 414 Dot1qCustomerSourceMacAddress
  decodeField 415 = decodeAs 415 Dot1qCustomerDestinationMacAddress
  decodeField 417 = decodeAs 417 PostLayer2OctetDeltaCount
  decodeField 418 = decodeAs 418 PostMCastLayer2OctetDeltaCount
  decodeField 420 = decodeAs 420 PostLayer2OctetTotalCount
  decodeField 421 = decodeAs 421 PostMCastLayer2OctetTotalCount
  decodeField 422 = decodeAs 422 MinimumLayer2TotalLength
  decodeField 423 = decodeAs 423 MaximumLayer2TotalLength
  decodeField 424 = decodeAs 424 DroppedLayer2OctetDeltaCount
  decodeField 425 = decodeAs 425 DroppedLayer2OctetTotalCount
  decodeField 426 = decodeAs 426 IgnoredLayer2OctetTotalCount
  decodeField 427 = decodeAs 427 NotSentLayer2OctetTotalCount
  decodeField 428 = decodeAs 428 Layer2OctetDeltaSumOfSquares
  decodeField 429 = decodeAs 429 Layer2OctetTotalSumOfSquares
  decodeField 430 = decodeAs 430 Layer2FrameDeltaCount
  decodeField 431 = decodeAs 431 Layer2FrameTotalCount
  decodeField 432 = decodeAs 432 PseudoWireDestinationIPv4Address
  decodeField 433 = decodeAs 433 IgnoredLayer2FrameTotalCount
  decodeField 434 = decodeAs 434 MibObjectValueInteger
  decodeField 435 = decodeAs 435 MibObjectValueOctetString
  decodeField 436 = decodeAs 436 MibObjectValueOID
  decodeField 437 = decodeAs 437 MibObjectValueBits
  decodeField 438 = decodeAs 438 MibObjectValueIPAddress
  decodeField 439 = decodeAs 439 MibObjectValueCounter
  decodeField 440 = decodeAs 440 MibObjectValueGauge
  decodeField 441 = decodeAs 441 MibObjectValueTimeTicks
  decodeField 442 = decodeAs 442 MibObjectValueUnsigned
  decodeField 445 = decodeAs 445 MibObjectIdentifier
  decodeField 446 = decodeAs 446 MibSubIdentifier
  decodeField 447 = decodeAs 447 MibIndexIndicator
  decodeField 448 = decodeAs 448 MibCaptureTimeSemantics
  decodeField 449 = decodeAs 449 MibContextEngineID
  decodeField 450 = decodeAs 450 MibContextName
  decodeField 451 = decodeAs 451 MibObjectName
  decodeField 452 = decodeAs 452 MibObjectDescription
  decodeField 453 = decodeAs 453 MibObjectSyntax
  decodeField 454 = decodeAs 454 MibModuleName
  decodeField 455 = decodeAs 455 MobileIMSI
  decodeField 456 = decodeAs 456 MobileMSISDN
  decodeField 457 = decodeAs 457 HttpStatusCode
  decodeField fid = OtherField fid


  instance ToJSON Field where
    toJSON = Object . jsonField


  instance ToJSON [Field] where
    toJSON = Object . foldl' union empty . map jsonField


  jsonField :: Field -> HashMap Text Value
  jsonField (OctetDeltaCount v)                       = singleton "octetDeltaCount"  (fieldToJSON v)
  jsonField (PacketDeltaCount v)                      = singleton "packetDeltaCount"  (fieldToJSON v)
  jsonField (DeltaFlowCount v)                        = singleton "deltaFlowCount"  (fieldToJSON v)
  jsonField (ProtocolIdentifier v)                    = singleton "protocolIdentifier"  (fieldToJSON v)
  jsonField (IpClassOfService v)                      = singleton "ipClassOfService"  (fieldToJSON v)
  jsonField (TcpControlBits v)                        = singleton "tcpControlBits"  (fieldToJSON v)
  jsonField (SourceTransportPort v)                   = singleton "sourceTransportPort"  (fieldToJSON v)
  jsonField (SourceIPv4Address v)                     = singleton "sourceIPv4Address"  (fieldToJSON v)
  jsonField (SourceIPv4PrefixLength v)                = singleton "sourceIPv4PrefixLength"  (fieldToJSON v)
  jsonField (IngressInterface v)                      = singleton "ingressInterface"  (fieldToJSON v)
  jsonField (DestinationTransportPort v)              = singleton "destinationTransportPort"  (fieldToJSON v)
  jsonField (DestinationIPv4Address v)                = singleton "destinationIPv4Address"  (fieldToJSON v)
  jsonField (DestinationIPv4PrefixLength v)           = singleton "destinationIPv4PrefixLength"  (fieldToJSON v)
  jsonField (EgressInterface v)                       = singleton "egressInterface"  (fieldToJSON v)
  jsonField (IpNextHopIPv4Address v)                  = singleton "ipNextHopIPv4Address"  (fieldToJSON v)
  jsonField (BgpSourceAsNumber v)                     = singleton "bgpSourceAsNumber"  (fieldToJSON v)
  jsonField (BgpDestinationAsNumber v)                = singleton "bgpDestinationAsNumber"  (fieldToJSON v)
  jsonField (BgpNextHopIPv4Address v)                 = singleton "bgpNextHopIPv4Address"  (fieldToJSON v)
  jsonField (PostMCastPacketDeltaCount v)             = singleton "postMCastPacketDeltaCount"  (fieldToJSON v)
  jsonField (PostMCastOctetDeltaCount v)              = singleton "postMCastOctetDeltaCount"  (fieldToJSON v)
  jsonField (FlowEndSysUpTime v)                      = singleton "flowEndSysUpTime"  (fieldToJSON v)
  jsonField (FlowStartSysUpTime v)                    = singleton "flowStartSysUpTime"  (fieldToJSON v)
  jsonField (PostOctetDeltaCount v)                   = singleton "postOctetDeltaCount"  (fieldToJSON v)
  jsonField (PostPacketDeltaCount v)                  = singleton "postPacketDeltaCount"  (fieldToJSON v)
  jsonField (MinimumIpTotalLength v)                  = singleton "minimumIpTotalLength"  (fieldToJSON v)
  jsonField (MaximumIpTotalLength v)                  = singleton "maximumIpTotalLength"  (fieldToJSON v)
  jsonField (SourceIPv6Address v)                     = singleton "sourceIPv6Address"  (fieldToJSON v)
  jsonField (DestinationIPv6Address v)                = singleton "destinationIPv6Address"  (fieldToJSON v)
  jsonField (SourceIPv6PrefixLength v)                = singleton "sourceIPv6PrefixLength"  (fieldToJSON v)
  jsonField (DestinationIPv6PrefixLength v)           = singleton "destinationIPv6PrefixLength"  (fieldToJSON v)
  jsonField (FlowLabelIPv6 v)                         = singleton "flowLabelIPv6"  (fieldToJSON v)
  jsonField (IcmpTypeCodeIPv4 v)                      = singleton "icmpTypeCodeIPv4"  (fieldToJSON v)
  jsonField (IgmpType v)                              = singleton "igmpType"  (fieldToJSON v)
  jsonField (SamplingInterval v)                      = singleton "samplingInterval"  (fieldToJSON v)
  jsonField (SamplingAlgorithm v)                     = singleton "samplingAlgorithm"  (fieldToJSON v)
  jsonField (FlowActiveTimeout v)                     = singleton "flowActiveTimeout"  (fieldToJSON v)
  jsonField (FlowIdleTimeout v)                       = singleton "flowIdleTimeout"  (fieldToJSON v)
  jsonField (EngineType v)                            = singleton "engineType"  (fieldToJSON v)
  jsonField (EngineId v)                              = singleton "engineId"  (fieldToJSON v)
  jsonField (ExportedOctetTotalCount v)               = singleton "exportedOctetTotalCount"  (fieldToJSON v)
  jsonField (ExportedMessageTotalCount v)             = singleton "exportedMessageTotalCount"  (fieldToJSON v)
  jsonField (ExportedFlowRecordTotalCount v)          = singleton "exportedFlowRecordTotalCount"  (fieldToJSON v)
  jsonField (Ipv4RouterSc v)                          = singleton "ipv4RouterSc"  (fieldToJSON v)
  jsonField (SourceIPv4Prefix v)                      = singleton "sourceIPv4Prefix"  (fieldToJSON v)
  jsonField (DestinationIPv4Prefix v)                 = singleton "destinationIPv4Prefix"  (fieldToJSON v)
  jsonField (MplsTopLabelType v)                      = singleton "mplsTopLabelType"  (fieldToJSON v)
  jsonField (MplsTopLabelIPv4Address v)               = singleton "mplsTopLabelIPv4Address"  (fieldToJSON v)
  jsonField (SamplerId v)                             = singleton "samplerId"  (fieldToJSON v)
  jsonField (SamplerMode v)                           = singleton "samplerMode"  (fieldToJSON v)
  jsonField (SamplerRandomInterval v)                 = singleton "samplerRandomInterval"  (fieldToJSON v)
  jsonField (ClassId v)                               = singleton "classId"  (fieldToJSON v)
  jsonField (MinimumTTL v)                            = singleton "minimumTTL"  (fieldToJSON v)
  jsonField (MaximumTTL v)                            = singleton "maximumTTL"  (fieldToJSON v)
  jsonField (FragmentIdentification v)                = singleton "fragmentIdentification"  (fieldToJSON v)
  jsonField (PostIpClassOfService v)                  = singleton "postIpClassOfService"  (fieldToJSON v)
  jsonField (SourceMacAddress v)                      = singleton "sourceMacAddress"  (fieldToJSON v)
  jsonField (PostDestinationMacAddress v)             = singleton "postDestinationMacAddress"  (fieldToJSON v)
  jsonField (VlanId v)                                = singleton "vlanId"  (fieldToJSON v)
  jsonField (PostVlanId v)                            = singleton "postVlanId"  (fieldToJSON v)
  jsonField (IpVersion v)                             = singleton "ipVersion"  (fieldToJSON v)
  jsonField (FlowDirection v)                         = singleton "flowDirection"  (fieldToJSON v)
  jsonField (IpNextHopIPv6Address v)                  = singleton "ipNextHopIPv6Address"  (fieldToJSON v)
  jsonField (BgpNextHopIPv6Address v)                 = singleton "bgpNextHopIPv6Address"  (fieldToJSON v)
  jsonField (Ipv6ExtensionHeaders v)                  = singleton "ipv6ExtensionHeaders"  (fieldToJSON v)
  jsonField (MplsTopLabelStackSection v)              = singleton "mplsTopLabelStackSection"  (fieldToJSON v)
  jsonField (MplsLabelStackSection2 v)                = singleton "mplsLabelStackSection2"  (fieldToJSON v)
  jsonField (MplsLabelStackSection3 v)                = singleton "mplsLabelStackSection3"  (fieldToJSON v)
  jsonField (MplsLabelStackSection4 v)                = singleton "mplsLabelStackSection4"  (fieldToJSON v)
  jsonField (MplsLabelStackSection5 v)                = singleton "mplsLabelStackSection5"  (fieldToJSON v)
  jsonField (MplsLabelStackSection6 v)                = singleton "mplsLabelStackSection6"  (fieldToJSON v)
  jsonField (MplsLabelStackSection7 v)                = singleton "mplsLabelStackSection7"  (fieldToJSON v)
  jsonField (MplsLabelStackSection8 v)                = singleton "mplsLabelStackSection8"  (fieldToJSON v)
  jsonField (MplsLabelStackSection9 v)                = singleton "mplsLabelStackSection9"  (fieldToJSON v)
  jsonField (MplsLabelStackSection10 v)               = singleton "mplsLabelStackSection10"  (fieldToJSON v)
  jsonField (DestinationMacAddress v)                 = singleton "destinationMacAddress"  (fieldToJSON v)
  jsonField (PostSourceMacAddress v)                  = singleton "postSourceMacAddress"  (fieldToJSON v)
  jsonField (InterfaceName v)                         = singleton "interfaceName"  (fieldToJSON v)
  jsonField (InterfaceDescription v)                  = singleton "interfaceDescription"  (fieldToJSON v)
  jsonField (SamplerName v)                           = singleton "samplerName"  (fieldToJSON v)
  jsonField (OctetTotalCount v)                       = singleton "octetTotalCount"  (fieldToJSON v)
  jsonField (PacketTotalCount v)                      = singleton "packetTotalCount"  (fieldToJSON v)
  jsonField (FlagsAndSamplerId v)                     = singleton "flagsAndSamplerId"  (fieldToJSON v)
  jsonField (FragmentOffset v)                        = singleton "fragmentOffset"  (fieldToJSON v)
  jsonField (ForwardingStatus v)                      = singleton "forwardingStatus"  (fieldToJSON v)
  jsonField (MplsVpnRouteDistinguisher v)             = singleton "mplsVpnRouteDistinguisher"  (fieldToJSON v)
  jsonField (MplsTopLabelPrefixLength v)              = singleton "mplsTopLabelPrefixLength"  (fieldToJSON v)
  jsonField (SrcTrafficIndex v)                       = singleton "srcTrafficIndex"  (fieldToJSON v)
  jsonField (DstTrafficIndex v)                       = singleton "dstTrafficIndex"  (fieldToJSON v)
  jsonField (ApplicationDescription v)                = singleton "applicationDescription"  (fieldToJSON v)
  jsonField (ApplicationId v)                         = singleton "applicationId"  (fieldToJSON v)
  jsonField (ApplicationName v)                       = singleton "applicationName"  (fieldToJSON v)
  jsonField (PostIpDiffServCodePoint v)               = singleton "postIpDiffServCodePoint"  (fieldToJSON v)
  jsonField (MulticastReplicationFactor v)            = singleton "multicastReplicationFactor"  (fieldToJSON v)
  jsonField (ClassName v)                             = singleton "className"  (fieldToJSON v)
  jsonField (ClassificationEngineId v)                = singleton "classificationEngineId"  (fieldToJSON v)
  jsonField (Layer2packetSectionOffset v)             = singleton "layer2packetSectionOffset"  (fieldToJSON v)
  jsonField (Layer2packetSectionSize v)               = singleton "layer2packetSectionSize"  (fieldToJSON v)
  jsonField (Layer2packetSectionData v)               = singleton "layer2packetSectionData"  (fieldToJSON v)
  jsonField (BgpNextAdjacentAsNumber v)               = singleton "bgpNextAdjacentAsNumber"  (fieldToJSON v)
  jsonField (BgpPrevAdjacentAsNumber v)               = singleton "bgpPrevAdjacentAsNumber"  (fieldToJSON v)
  jsonField (ExporterIPv4Address v)                   = singleton "exporterIPv4Address"  (fieldToJSON v)
  jsonField (ExporterIPv6Address v)                   = singleton "exporterIPv6Address"  (fieldToJSON v)
  jsonField (DroppedOctetDeltaCount v)                = singleton "droppedOctetDeltaCount"  (fieldToJSON v)
  jsonField (DroppedPacketDeltaCount v)               = singleton "droppedPacketDeltaCount"  (fieldToJSON v)
  jsonField (DroppedOctetTotalCount v)                = singleton "droppedOctetTotalCount"  (fieldToJSON v)
  jsonField (DroppedPacketTotalCount v)               = singleton "droppedPacketTotalCount"  (fieldToJSON v)
  jsonField (FlowEndReason v)                         = singleton "flowEndReason"  (fieldToJSON v)
  jsonField (CommonPropertiesId v)                    = singleton "commonPropertiesId"  (fieldToJSON v)
  jsonField (ObservationPointId v)                    = singleton "observationPointId"  (fieldToJSON v)
  jsonField (IcmpTypeCodeIPv6 v)                      = singleton "icmpTypeCodeIPv6"  (fieldToJSON v)
  jsonField (MplsTopLabelIPv6Address v)               = singleton "mplsTopLabelIPv6Address"  (fieldToJSON v)
  jsonField (LineCardId v)                            = singleton "lineCardId"  (fieldToJSON v)
  jsonField (PortId v)                                = singleton "portId"  (fieldToJSON v)
  jsonField (MeteringProcessId v)                     = singleton "meteringProcessId"  (fieldToJSON v)
  jsonField (ExportingProcessId v)                    = singleton "exportingProcessId"  (fieldToJSON v)
  jsonField (TemplateId v)                            = singleton "templateId"  (fieldToJSON v)
  jsonField (WlanChannelId v)                         = singleton "wlanChannelId"  (fieldToJSON v)
  jsonField (WlanSSID v)                              = singleton "wlanSSID"  (fieldToJSON v)
  jsonField (FlowId v)                                = singleton "flowId"  (fieldToJSON v)
  jsonField (ObservationDomainId v)                   = singleton "observationDomainId"  (fieldToJSON v)
  jsonField (FlowStartSeconds v)                      = singleton "flowStartSeconds"  (fieldToJSON v)
  jsonField (FlowEndSeconds v)                        = singleton "flowEndSeconds"  (fieldToJSON v)
  jsonField (FlowStartMilliseconds v)                 = singleton "flowStartMilliseconds"  (fieldToJSON v)
  jsonField (FlowEndMilliseconds v)                   = singleton "flowEndMilliseconds"  (fieldToJSON v)
  jsonField (FlowStartMicroseconds v)                 = singleton "flowStartMicroseconds"  (fieldToJSON v)
  jsonField (FlowEndMicroseconds v)                   = singleton "flowEndMicroseconds"  (fieldToJSON v)
  jsonField (FlowStartNanoseconds v)                  = singleton "flowStartNanoseconds"  (fieldToJSON v)
  jsonField (FlowEndNanoseconds v)                    = singleton "flowEndNanoseconds"  (fieldToJSON v)
  jsonField (FlowStartDeltaMicroseconds v)            = singleton "flowStartDeltaMicroseconds"  (fieldToJSON v)
  jsonField (FlowEndDeltaMicroseconds v)              = singleton "flowEndDeltaMicroseconds"  (fieldToJSON v)
  jsonField (SystemInitTimeMilliseconds v)            = singleton "systemInitTimeMilliseconds"  (fieldToJSON v)
  jsonField (FlowDurationMilliseconds v)              = singleton "flowDurationMilliseconds"  (fieldToJSON v)
  jsonField (FlowDurationMicroseconds v)              = singleton "flowDurationMicroseconds"  (fieldToJSON v)
  jsonField (ObservedFlowTotalCount v)                = singleton "observedFlowTotalCount"  (fieldToJSON v)
  jsonField (IgnoredPacketTotalCount v)               = singleton "ignoredPacketTotalCount"  (fieldToJSON v)
  jsonField (IgnoredOctetTotalCount v)                = singleton "ignoredOctetTotalCount"  (fieldToJSON v)
  jsonField (NotSentFlowTotalCount v)                 = singleton "notSentFlowTotalCount"  (fieldToJSON v)
  jsonField (NotSentPacketTotalCount v)               = singleton "notSentPacketTotalCount"  (fieldToJSON v)
  jsonField (NotSentOctetTotalCount v)                = singleton "notSentOctetTotalCount"  (fieldToJSON v)
  jsonField (DestinationIPv6Prefix v)                 = singleton "destinationIPv6Prefix"  (fieldToJSON v)
  jsonField (SourceIPv6Prefix v)                      = singleton "sourceIPv6Prefix"  (fieldToJSON v)
  jsonField (PostOctetTotalCount v)                   = singleton "postOctetTotalCount"  (fieldToJSON v)
  jsonField (PostPacketTotalCount v)                  = singleton "postPacketTotalCount"  (fieldToJSON v)
  jsonField (FlowKeyIndicator v)                      = singleton "flowKeyIndicator"  (fieldToJSON v)
  jsonField (PostMCastPacketTotalCount v)             = singleton "postMCastPacketTotalCount"  (fieldToJSON v)
  jsonField (PostMCastOctetTotalCount v)              = singleton "postMCastOctetTotalCount"  (fieldToJSON v)
  jsonField (IcmpTypeIPv4 v)                          = singleton "icmpTypeIPv4"  (fieldToJSON v)
  jsonField (IcmpCodeIPv4 v)                          = singleton "icmpCodeIPv4"  (fieldToJSON v)
  jsonField (IcmpTypeIPv6 v)                          = singleton "icmpTypeIPv6"  (fieldToJSON v)
  jsonField (IcmpCodeIPv6 v)                          = singleton "icmpCodeIPv6"  (fieldToJSON v)
  jsonField (UdpSourcePort v)                         = singleton "udpSourcePort"  (fieldToJSON v)
  jsonField (UdpDestinationPort v)                    = singleton "udpDestinationPort"  (fieldToJSON v)
  jsonField (TcpSourcePort v)                         = singleton "tcpSourcePort"  (fieldToJSON v)
  jsonField (TcpDestinationPort v)                    = singleton "tcpDestinationPort"  (fieldToJSON v)
  jsonField (TcpSequenceNumber v)                     = singleton "tcpSequenceNumber"  (fieldToJSON v)
  jsonField (TcpAcknowledgementNumber v)              = singleton "tcpAcknowledgementNumber"  (fieldToJSON v)
  jsonField (TcpWindowSize v)                         = singleton "tcpWindowSize"  (fieldToJSON v)
  jsonField (TcpUrgentPointer v)                      = singleton "tcpUrgentPointer"  (fieldToJSON v)
  jsonField (TcpHeaderLength v)                       = singleton "tcpHeaderLength"  (fieldToJSON v)
  jsonField (IpHeaderLength v)                        = singleton "ipHeaderLength"  (fieldToJSON v)
  jsonField (TotalLengthIPv4 v)                       = singleton "totalLengthIPv4"  (fieldToJSON v)
  jsonField (PayloadLengthIPv6 v)                     = singleton "payloadLengthIPv6"  (fieldToJSON v)
  jsonField (IpTTL v)                                 = singleton "ipTTL"  (fieldToJSON v)
  jsonField (NextHeaderIPv6 v)                        = singleton "nextHeaderIPv6"  (fieldToJSON v)
  jsonField (MplsPayloadLength v)                     = singleton "mplsPayloadLength"  (fieldToJSON v)
  jsonField (IpDiffServCodePoint v)                   = singleton "ipDiffServCodePoint"  (fieldToJSON v)
  jsonField (IpPrecedence v)                          = singleton "ipPrecedence"  (fieldToJSON v)
  jsonField (FragmentFlags v)                         = singleton "fragmentFlags"  (fieldToJSON v)
  jsonField (OctetDeltaSumOfSquares v)                = singleton "octetDeltaSumOfSquares"  (fieldToJSON v)
  jsonField (OctetTotalSumOfSquares v)                = singleton "octetTotalSumOfSquares"  (fieldToJSON v)
  jsonField (MplsTopLabelTTL v)                       = singleton "mplsTopLabelTTL"  (fieldToJSON v)
  jsonField (MplsLabelStackLength v)                  = singleton "mplsLabelStackLength"  (fieldToJSON v)
  jsonField (MplsLabelStackDepth v)                   = singleton "mplsLabelStackDepth"  (fieldToJSON v)
  jsonField (MplsTopLabelExp v)                       = singleton "mplsTopLabelExp"  (fieldToJSON v)
  jsonField (IpPayloadLength v)                       = singleton "ipPayloadLength"  (fieldToJSON v)
  jsonField (UdpMessageLength v)                      = singleton "udpMessageLength"  (fieldToJSON v)
  jsonField (IsMulticast v)                           = singleton "isMulticast"  (fieldToJSON v)
  jsonField (Ipv4IHL v)                               = singleton "ipv4IHL"  (fieldToJSON v)
  jsonField (Ipv4Options v)                           = singleton "ipv4Options"  (fieldToJSON v)
  jsonField (TcpOptions v)                            = singleton "tcpOptions"  (fieldToJSON v)
  jsonField (PaddingOctets v)                         = singleton "paddingOctets"  (fieldToJSON v)
  jsonField (CollectorIPv4Address v)                  = singleton "collectorIPv4Address"  (fieldToJSON v)
  jsonField (CollectorIPv6Address v)                  = singleton "collectorIPv6Address"  (fieldToJSON v)
  jsonField (ExportInterface v)                       = singleton "exportInterface"  (fieldToJSON v)
  jsonField (ExportProtocolVersion v)                 = singleton "exportProtocolVersion"  (fieldToJSON v)
  jsonField (ExportTransportProtocol v)               = singleton "exportTransportProtocol"  (fieldToJSON v)
  jsonField (CollectorTransportPort v)                = singleton "collectorTransportPort"  (fieldToJSON v)
  jsonField (ExporterTransportPort v)                 = singleton "exporterTransportPort"  (fieldToJSON v)
  jsonField (TcpSynTotalCount v)                      = singleton "tcpSynTotalCount"  (fieldToJSON v)
  jsonField (TcpFinTotalCount v)                      = singleton "tcpFinTotalCount"  (fieldToJSON v)
  jsonField (TcpRstTotalCount v)                      = singleton "tcpRstTotalCount"  (fieldToJSON v)
  jsonField (TcpPshTotalCount v)                      = singleton "tcpPshTotalCount"  (fieldToJSON v)
  jsonField (TcpAckTotalCount v)                      = singleton "tcpAckTotalCount"  (fieldToJSON v)
  jsonField (TcpUrgTotalCount v)                      = singleton "tcpUrgTotalCount"  (fieldToJSON v)
  jsonField (IpTotalLength v)                         = singleton "ipTotalLength"  (fieldToJSON v)
  jsonField (PostNATSourceIPv4Address v)              = singleton "postNATSourceIPv4Address"  (fieldToJSON v)
  jsonField (PostNATDestinationIPv4Address v)         = singleton "postNATDestinationIPv4Address"  (fieldToJSON v)
  jsonField (PostNAPTSourceTransportPort v)           = singleton "postNAPTSourceTransportPort"  (fieldToJSON v)
  jsonField (PostNAPTDestinationTransportPort v)      = singleton "postNAPTDestinationTransportPort"  (fieldToJSON v)
  jsonField (NatOriginatingAddressRealm v)            = singleton "natOriginatingAddressRealm"  (fieldToJSON v)
  jsonField (NatEvent v)                              = singleton "natEvent"  (fieldToJSON v)
  jsonField (InitiatorOctets v)                       = singleton "initiatorOctets"  (fieldToJSON v)
  jsonField (ResponderOctets v)                       = singleton "responderOctets"  (fieldToJSON v)
  jsonField (FirewallEvent v)                         = singleton "firewallEvent"  (fieldToJSON v)
  jsonField (IngressVRFID v)                          = singleton "ingressVRFID"  (fieldToJSON v)
  jsonField (EgressVRFID v)                           = singleton "egressVRFID"  (fieldToJSON v)
  jsonField (VRFname v)                               = singleton "vRFname"  (fieldToJSON v)
  jsonField (PostMplsTopLabelExp v)                   = singleton "postMplsTopLabelExp"  (fieldToJSON v)
  jsonField (TcpWindowScale v)                        = singleton "tcpWindowScale"  (fieldToJSON v)
  jsonField (BiflowDirection v)                       = singleton "biflowDirection"  (fieldToJSON v)
  jsonField (EthernetHeaderLength v)                  = singleton "ethernetHeaderLength"  (fieldToJSON v)
  jsonField (EthernetPayloadLength v)                 = singleton "ethernetPayloadLength"  (fieldToJSON v)
  jsonField (EthernetTotalLength v)                   = singleton "ethernetTotalLength"  (fieldToJSON v)
  jsonField (Dot1qVlanId v)                           = singleton "dot1qVlanId"  (fieldToJSON v)
  jsonField (Dot1qPriority v)                         = singleton "dot1qPriority"  (fieldToJSON v)
  jsonField (Dot1qCustomerVlanId v)                   = singleton "dot1qCustomerVlanId"  (fieldToJSON v)
  jsonField (Dot1qCustomerPriority v)                 = singleton "dot1qCustomerPriority"  (fieldToJSON v)
  jsonField (MetroEvcId v)                            = singleton "metroEvcId"  (fieldToJSON v)
  jsonField (MetroEvcType v)                          = singleton "metroEvcType"  (fieldToJSON v)
  jsonField (PseudoWireId v)                          = singleton "pseudoWireId"  (fieldToJSON v)
  jsonField (PseudoWireType v)                        = singleton "pseudoWireType"  (fieldToJSON v)
  jsonField (PseudoWireControlWord v)                 = singleton "pseudoWireControlWord"  (fieldToJSON v)
  jsonField (IngressPhysicalInterface v)              = singleton "ingressPhysicalInterface"  (fieldToJSON v)
  jsonField (EgressPhysicalInterface v)               = singleton "egressPhysicalInterface"  (fieldToJSON v)
  jsonField (PostDot1qVlanId v)                       = singleton "postDot1qVlanId"  (fieldToJSON v)
  jsonField (PostDot1qCustomerVlanId v)               = singleton "postDot1qCustomerVlanId"  (fieldToJSON v)
  jsonField (EthernetType v)                          = singleton "ethernetType"  (fieldToJSON v)
  jsonField (PostIpPrecedence v)                      = singleton "postIpPrecedence"  (fieldToJSON v)
  jsonField (CollectionTimeMilliseconds v)            = singleton "collectionTimeMilliseconds"  (fieldToJSON v)
  jsonField (ExportSctpStreamId v)                    = singleton "exportSctpStreamId"  (fieldToJSON v)
  jsonField (MaxExportSeconds v)                      = singleton "maxExportSeconds"  (fieldToJSON v)
  jsonField (MaxFlowEndSeconds v)                     = singleton "maxFlowEndSeconds"  (fieldToJSON v)
  jsonField (MessageMD5Checksum v)                    = singleton "messageMD5Checksum"  (fieldToJSON v)
  jsonField (MessageScope v)                          = singleton "messageScope"  (fieldToJSON v)
  jsonField (MinExportSeconds v)                      = singleton "minExportSeconds"  (fieldToJSON v)
  jsonField (MinFlowStartSeconds v)                   = singleton "minFlowStartSeconds"  (fieldToJSON v)
  jsonField (OpaqueOctets v)                          = singleton "opaqueOctets"  (fieldToJSON v)
  jsonField (SessionScope v)                          = singleton "sessionScope"  (fieldToJSON v)
  jsonField (MaxFlowEndMicroseconds v)                = singleton "maxFlowEndMicroseconds"  (fieldToJSON v)
  jsonField (MaxFlowEndMilliseconds v)                = singleton "maxFlowEndMilliseconds"  (fieldToJSON v)
  jsonField (MaxFlowEndNanoseconds v)                 = singleton "maxFlowEndNanoseconds"  (fieldToJSON v)
  jsonField (MinFlowStartMicroseconds v)              = singleton "minFlowStartMicroseconds"  (fieldToJSON v)
  jsonField (MinFlowStartMilliseconds v)              = singleton "minFlowStartMilliseconds"  (fieldToJSON v)
  jsonField (MinFlowStartNanoseconds v)               = singleton "minFlowStartNanoseconds"  (fieldToJSON v)
  jsonField (CollectorCertificate v)                  = singleton "collectorCertificate"  (fieldToJSON v)
  jsonField (ExporterCertificate v)                   = singleton "exporterCertificate"  (fieldToJSON v)
  jsonField (DataRecordsReliability v)                = singleton "dataRecordsReliability"  (fieldToJSON v)
  jsonField (ObservationPointType v)                  = singleton "observationPointType"  (fieldToJSON v)
  jsonField (NewConnectionDeltaCount v)               = singleton "newConnectionDeltaCount"  (fieldToJSON v)
  jsonField (ConnectionSumDurationSeconds v)          = singleton "connectionSumDurationSeconds"  (fieldToJSON v)
  jsonField (ConnectionTransactionId v)               = singleton "connectionTransactionId"  (fieldToJSON v)
  jsonField (PostNATSourceIPv6Address v)              = singleton "postNATSourceIPv6Address"  (fieldToJSON v)
  jsonField (PostNATDestinationIPv6Address v)         = singleton "postNATDestinationIPv6Address"  (fieldToJSON v)
  jsonField (NatPoolId v)                             = singleton "natPoolId"  (fieldToJSON v)
  jsonField (NatPoolName v)                           = singleton "natPoolName"  (fieldToJSON v)
  jsonField (AnonymizationFlags v)                    = singleton "anonymizationFlags"  (fieldToJSON v)
  jsonField (AnonymizationTechnique v)                = singleton "anonymizationTechnique"  (fieldToJSON v)
  jsonField (InformationElementIndex v)               = singleton "informationElementIndex"  (fieldToJSON v)
  jsonField (P2pTechnology v)                         = singleton "p2pTechnology"  (fieldToJSON v)
  jsonField (TunnelTechnology v)                      = singleton "tunnelTechnology"  (fieldToJSON v)
  jsonField (EncryptedTechnology v)                   = singleton "encryptedTechnology"  (fieldToJSON v)
  jsonField (BgpValidityState v)                      = singleton "bgpValidityState"  (fieldToJSON v)
  jsonField (IPSecSPI v)                              = singleton "iPSecSPI"  (fieldToJSON v)
  jsonField (GreKey v)                                = singleton "greKey"  (fieldToJSON v)
  jsonField (NatType v)                               = singleton "natType"  (fieldToJSON v)
  jsonField (InitiatorPackets v)                      = singleton "initiatorPackets"  (fieldToJSON v)
  jsonField (ResponderPackets v)                      = singleton "responderPackets"  (fieldToJSON v)
  jsonField (ObservationDomainName v)                 = singleton "observationDomainName"  (fieldToJSON v)
  jsonField (SelectionSequenceId v)                   = singleton "selectionSequenceId"  (fieldToJSON v)
  jsonField (SelectorId v)                            = singleton "selectorId"  (fieldToJSON v)
  jsonField (InformationElementId v)                  = singleton "informationElementId"  (fieldToJSON v)
  jsonField (SelectorAlgorithm v)                     = singleton "selectorAlgorithm"  (fieldToJSON v)
  jsonField (SamplingPacketInterval v)                = singleton "samplingPacketInterval"  (fieldToJSON v)
  jsonField (SamplingPacketSpace v)                   = singleton "samplingPacketSpace"  (fieldToJSON v)
  jsonField (SamplingTimeInterval v)                  = singleton "samplingTimeInterval"  (fieldToJSON v)
  jsonField (SamplingTimeSpace v)                     = singleton "samplingTimeSpace"  (fieldToJSON v)
  jsonField (SamplingSize v)                          = singleton "samplingSize"  (fieldToJSON v)
  jsonField (SamplingPopulation v)                    = singleton "samplingPopulation"  (fieldToJSON v)
  jsonField (SamplingProbability v)                   = singleton "samplingProbability"  (fieldToJSON v)
  jsonField (DataLinkFrameSize v)                     = singleton "dataLinkFrameSize"  (fieldToJSON v)
  jsonField (IpHeaderPacketSection v)                 = singleton "ipHeaderPacketSection"  (fieldToJSON v)
  jsonField (IpPayloadPacketSection v)                = singleton "ipPayloadPacketSection"  (fieldToJSON v)
  jsonField (DataLinkFrameSection v)                  = singleton "dataLinkFrameSection"  (fieldToJSON v)
  jsonField (MplsLabelStackSection v)                 = singleton "mplsLabelStackSection"  (fieldToJSON v)
  jsonField (MplsPayloadPacketSection v)              = singleton "mplsPayloadPacketSection"  (fieldToJSON v)
  jsonField (SelectorIdTotalPktsObserved v)           = singleton "selectorIdTotalPktsObserved"  (fieldToJSON v)
  jsonField (SelectorIdTotalPktsSelected v)           = singleton "selectorIdTotalPktsSelected"  (fieldToJSON v)
  jsonField (AbsoluteError v)                         = singleton "absoluteError"  (fieldToJSON v)
  jsonField (RelativeError v)                         = singleton "relativeError"  (fieldToJSON v)
  jsonField (ObservationTimeSeconds v)                = singleton "observationTimeSeconds"  (fieldToJSON v)
  jsonField (ObservationTimeMilliseconds v)           = singleton "observationTimeMilliseconds"  (fieldToJSON v)
  jsonField (ObservationTimeMicroseconds v)           = singleton "observationTimeMicroseconds"  (fieldToJSON v)
  jsonField (ObservationTimeNanoseconds v)            = singleton "observationTimeNanoseconds"  (fieldToJSON v)
  jsonField (DigestHashValue v)                       = singleton "digestHashValue"  (fieldToJSON v)
  jsonField (HashIPPayloadOffset v)                   = singleton "hashIPPayloadOffset"  (fieldToJSON v)
  jsonField (HashIPPayloadSize v)                     = singleton "hashIPPayloadSize"  (fieldToJSON v)
  jsonField (HashOutputRangeMin v)                    = singleton "hashOutputRangeMin"  (fieldToJSON v)
  jsonField (HashOutputRangeMax v)                    = singleton "hashOutputRangeMax"  (fieldToJSON v)
  jsonField (HashSelectedRangeMin v)                  = singleton "hashSelectedRangeMin"  (fieldToJSON v)
  jsonField (HashSelectedRangeMax v)                  = singleton "hashSelectedRangeMax"  (fieldToJSON v)
  jsonField (HashDigestOutput v)                      = singleton "hashDigestOutput"  (fieldToJSON v)
  jsonField (HashInitialiserValue v)                  = singleton "hashInitialiserValue"  (fieldToJSON v)
  jsonField (SelectorName v)                          = singleton "selectorName"  (fieldToJSON v)
  jsonField (UpperCILimit v)                          = singleton "upperCILimit"  (fieldToJSON v)
  jsonField (LowerCILimit v)                          = singleton "lowerCILimit"  (fieldToJSON v)
  jsonField (ConfidenceLevel v)                       = singleton "confidenceLevel"  (fieldToJSON v)
  jsonField (InformationElementDataType v)            = singleton "informationElementDataType"  (fieldToJSON v)
  jsonField (InformationElementDescription v)         = singleton "informationElementDescription"  (fieldToJSON v)
  jsonField (InformationElementName v)                = singleton "informationElementName"  (fieldToJSON v)
  jsonField (InformationElementRangeBegin v)          = singleton "informationElementRangeBegin"  (fieldToJSON v)
  jsonField (InformationElementRangeEnd v)            = singleton "informationElementRangeEnd"  (fieldToJSON v)
  jsonField (InformationElementSemantics v)           = singleton "informationElementSemantics"  (fieldToJSON v)
  jsonField (InformationElementUnits v)               = singleton "informationElementUnits"  (fieldToJSON v)
  jsonField (PrivateEnterpriseNumber v)               = singleton "privateEnterpriseNumber"  (fieldToJSON v)
  jsonField (VirtualStationInterfaceId v)             = singleton "virtualStationInterfaceId"  (fieldToJSON v)
  jsonField (VirtualStationInterfaceName v)           = singleton "virtualStationInterfaceName"  (fieldToJSON v)
  jsonField (VirtualStationUUID v)                    = singleton "virtualStationUUID"  (fieldToJSON v)
  jsonField (VirtualStationName v)                    = singleton "virtualStationName"  (fieldToJSON v)
  jsonField (Layer2SegmentId v)                       = singleton "layer2SegmentId"  (fieldToJSON v)
  jsonField (Layer2OctetDeltaCount v)                 = singleton "layer2OctetDeltaCount"  (fieldToJSON v)
  jsonField (Layer2OctetTotalCount v)                 = singleton "layer2OctetTotalCount"  (fieldToJSON v)
  jsonField (IngressUnicastPacketTotalCount v)        = singleton "ingressUnicastPacketTotalCount"  (fieldToJSON v)
  jsonField (IngressMulticastPacketTotalCount v)      = singleton "ingressMulticastPacketTotalCount"  (fieldToJSON v)
  jsonField (IngressBroadcastPacketTotalCount v)      = singleton "ingressBroadcastPacketTotalCount"  (fieldToJSON v)
  jsonField (EgressUnicastPacketTotalCount v)         = singleton "egressUnicastPacketTotalCount"  (fieldToJSON v)
  jsonField (EgressBroadcastPacketTotalCount v)       = singleton "egressBroadcastPacketTotalCount"  (fieldToJSON v)
  jsonField (MonitoringIntervalStartMilliSeconds v)   = singleton "monitoringIntervalStartMilliSeconds"  (fieldToJSON v)
  jsonField (MonitoringIntervalEndMilliSeconds v)     = singleton "monitoringIntervalEndMilliSeconds"  (fieldToJSON v)
  jsonField (PortRangeStart v)                        = singleton "portRangeStart"  (fieldToJSON v)
  jsonField (PortRangeEnd v)                          = singleton "portRangeEnd"  (fieldToJSON v)
  jsonField (PortRangeStepSize v)                     = singleton "portRangeStepSize"  (fieldToJSON v)
  jsonField (PortRangeNumPorts v)                     = singleton "portRangeNumPorts"  (fieldToJSON v)
  jsonField (StaMacAddress v)                         = singleton "staMacAddress"  (fieldToJSON v)
  jsonField (StaIPv4Address v)                        = singleton "staIPv4Address"  (fieldToJSON v)
  jsonField (WtpMacAddress v)                         = singleton "wtpMacAddress"  (fieldToJSON v)
  jsonField (IngressInterfaceType v)                  = singleton "ingressInterfaceType"  (fieldToJSON v)
  jsonField (EgressInterfaceType v)                   = singleton "egressInterfaceType"  (fieldToJSON v)
  jsonField (RtpSequenceNumber v)                     = singleton "rtpSequenceNumber"  (fieldToJSON v)
  jsonField (UserName v)                              = singleton "userName"  (fieldToJSON v)
  jsonField (ApplicationCategoryName v)               = singleton "applicationCategoryName"  (fieldToJSON v)
  jsonField (ApplicationSubCategoryName v)            = singleton "applicationSubCategoryName"  (fieldToJSON v)
  jsonField (ApplicationGroupName v)                  = singleton "applicationGroupName"  (fieldToJSON v)
  jsonField (OriginalFlowsPresent v)                  = singleton "originalFlowsPresent"  (fieldToJSON v)
  jsonField (OriginalFlowsInitiated v)                = singleton "originalFlowsInitiated"  (fieldToJSON v)
  jsonField (OriginalFlowsCompleted v)                = singleton "originalFlowsCompleted"  (fieldToJSON v)
  jsonField (DistinctCountOfSourceIPAddress v)        = singleton "distinctCountOfSourceIPAddress"  (fieldToJSON v)
  jsonField (DistinctCountOfDestinationIPAddress v)   = singleton "distinctCountOfDestinationIPAddress"  (fieldToJSON v)
  jsonField (DistinctCountOfSourceIPv4Address v)      = singleton "distinctCountOfSourceIPv4Address"  (fieldToJSON v)
  jsonField (DistinctCountOfDestinationIPv4Address v) = singleton "distinctCountOfDestinationIPv4Address"  (fieldToJSON v)
  jsonField (DistinctCountOfSourceIPv6Address v)      = singleton "distinctCountOfSourceIPv6Address"  (fieldToJSON v)
  jsonField (DistinctCountOfDestinationIPv6Address v) = singleton "distinctCountOfDestinationIPv6Address"  (fieldToJSON v)
  jsonField (ValueDistributionMethod v)               = singleton "valueDistributionMethod"  (fieldToJSON v)
  jsonField (Rfc3550JitterMilliseconds v)             = singleton "rfc3550JitterMilliseconds"  (fieldToJSON v)
  jsonField (Rfc3550JitterMicroseconds v)             = singleton "rfc3550JitterMicroseconds"  (fieldToJSON v)
  jsonField (Rfc3550JitterNanoseconds v)              = singleton "rfc3550JitterNanoseconds"  (fieldToJSON v)
  jsonField (Dot1qDEI v)                              = singleton "dot1qDEI"  (fieldToJSON v)
  jsonField (Dot1qCustomerDEI v)                      = singleton "dot1qCustomerDEI"  (fieldToJSON v)
  jsonField (FlowSelectorAlgorithm v)                 = singleton "flowSelectorAlgorithm"  (fieldToJSON v)
  jsonField (FlowSelectedOctetDeltaCount v)           = singleton "flowSelectedOctetDeltaCount"  (fieldToJSON v)
  jsonField (FlowSelectedPacketDeltaCount v)          = singleton "flowSelectedPacketDeltaCount"  (fieldToJSON v)
  jsonField (FlowSelectedFlowDeltaCount v)            = singleton "flowSelectedFlowDeltaCount"  (fieldToJSON v)
  jsonField (SelectorIDTotalFlowsObserved v)          = singleton "selectorIDTotalFlowsObserved"  (fieldToJSON v)
  jsonField (SelectorIDTotalFlowsSelected v)          = singleton "selectorIDTotalFlowsSelected"  (fieldToJSON v)
  jsonField (SamplingFlowInterval v)                  = singleton "samplingFlowInterval"  (fieldToJSON v)
  jsonField (SamplingFlowSpacing v)                   = singleton "samplingFlowSpacing"  (fieldToJSON v)
  jsonField (FlowSamplingTimeInterval v)              = singleton "flowSamplingTimeInterval"  (fieldToJSON v)
  jsonField (FlowSamplingTimeSpacing v)               = singleton "flowSamplingTimeSpacing"  (fieldToJSON v)
  jsonField (HashFlowDomain v)                        = singleton "hashFlowDomain"  (fieldToJSON v)
  jsonField (TransportOctetDeltaCount v)              = singleton "transportOctetDeltaCount"  (fieldToJSON v)
  jsonField (TransportPacketDeltaCount v)             = singleton "transportPacketDeltaCount"  (fieldToJSON v)
  jsonField (OriginalExporterIPv4Address v)           = singleton "originalExporterIPv4Address"  (fieldToJSON v)
  jsonField (OriginalExporterIPv6Address v)           = singleton "originalExporterIPv6Address"  (fieldToJSON v)
  jsonField (OriginalObservationDomainId v)           = singleton "originalObservationDomainId"  (fieldToJSON v)
  jsonField (IntermediateProcessId v)                 = singleton "intermediateProcessId"  (fieldToJSON v)
  jsonField (IgnoredDataRecordTotalCount v)           = singleton "ignoredDataRecordTotalCount"  (fieldToJSON v)
  jsonField (DataLinkFrameType v)                     = singleton "dataLinkFrameType"  (fieldToJSON v)
  jsonField (SectionOffset v)                         = singleton "sectionOffset"  (fieldToJSON v)
  jsonField (SectionExportedOctets v)                 = singleton "sectionExportedOctets"  (fieldToJSON v)
  jsonField (Dot1qServiceInstanceTag v)               = singleton "dot1qServiceInstanceTag"  (fieldToJSON v)
  jsonField (Dot1qServiceInstanceId v)                = singleton "dot1qServiceInstanceId"  (fieldToJSON v)
  jsonField (Dot1qServiceInstancePriority v)          = singleton "dot1qServiceInstancePriority"  (fieldToJSON v)
  jsonField (Dot1qCustomerSourceMacAddress v)         = singleton "dot1qCustomerSourceMacAddress"  (fieldToJSON v)
  jsonField (Dot1qCustomerDestinationMacAddress v)    = singleton "dot1qCustomerDestinationMacAddress"  (fieldToJSON v)
  jsonField (PostLayer2OctetDeltaCount v)             = singleton "postLayer2OctetDeltaCount"  (fieldToJSON v)
  jsonField (PostMCastLayer2OctetDeltaCount v)        = singleton "postMCastLayer2OctetDeltaCount"  (fieldToJSON v)
  jsonField (PostLayer2OctetTotalCount v)             = singleton "postLayer2OctetTotalCount"  (fieldToJSON v)
  jsonField (PostMCastLayer2OctetTotalCount v)        = singleton "postMCastLayer2OctetTotalCount"  (fieldToJSON v)
  jsonField (MinimumLayer2TotalLength v)              = singleton "minimumLayer2TotalLength"  (fieldToJSON v)
  jsonField (MaximumLayer2TotalLength v)              = singleton "maximumLayer2TotalLength"  (fieldToJSON v)
  jsonField (DroppedLayer2OctetDeltaCount v)          = singleton "droppedLayer2OctetDeltaCount"  (fieldToJSON v)
  jsonField (DroppedLayer2OctetTotalCount v)          = singleton "droppedLayer2OctetTotalCount"  (fieldToJSON v)
  jsonField (IgnoredLayer2OctetTotalCount v)          = singleton "ignoredLayer2OctetTotalCount"  (fieldToJSON v)
  jsonField (NotSentLayer2OctetTotalCount v)          = singleton "notSentLayer2OctetTotalCount"  (fieldToJSON v)
  jsonField (Layer2OctetDeltaSumOfSquares v)          = singleton "layer2OctetDeltaSumOfSquares"  (fieldToJSON v)
  jsonField (Layer2OctetTotalSumOfSquares v)          = singleton "layer2OctetTotalSumOfSquares"  (fieldToJSON v)
  jsonField (Layer2FrameDeltaCount v)                 = singleton "layer2FrameDeltaCount"  (fieldToJSON v)
  jsonField (Layer2FrameTotalCount v)                 = singleton "layer2FrameTotalCount"  (fieldToJSON v)
  jsonField (PseudoWireDestinationIPv4Address v)      = singleton "pseudoWireDestinationIPv4Address"  (fieldToJSON v)
  jsonField (IgnoredLayer2FrameTotalCount v)          = singleton "ignoredLayer2FrameTotalCount"  (fieldToJSON v)
  jsonField (MibObjectValueInteger v)                 = singleton "mibObjectValueInteger"  (fieldToJSON v)
  jsonField (MibObjectValueOctetString v)             = singleton "mibObjectValueOctetString"  (fieldToJSON v)
  jsonField (MibObjectValueOID v)                     = singleton "mibObjectValueOID"  (fieldToJSON v)
  jsonField (MibObjectValueBits v)                    = singleton "mibObjectValueBits"  (fieldToJSON v)
  jsonField (MibObjectValueIPAddress v)               = singleton "mibObjectValueIPAddress"  (fieldToJSON v)
  jsonField (MibObjectValueCounter v)                 = singleton "mibObjectValueCounter"  (fieldToJSON v)
  jsonField (MibObjectValueGauge v)                   = singleton "mibObjectValueGauge"  (fieldToJSON v)
  jsonField (MibObjectValueTimeTicks v)               = singleton "mibObjectValueTimeTicks"  (fieldToJSON v)
  jsonField (MibObjectValueUnsigned v)                = singleton "mibObjectValueUnsigned"  (fieldToJSON v)
  jsonField (MibObjectIdentifier v)                   = singleton "mibObjectIdentifier"  (fieldToJSON v)
  jsonField (MibSubIdentifier v)                      = singleton "mibSubIdentifier"  (fieldToJSON v)
  jsonField (MibIndexIndicator v)                     = singleton "mibIndexIndicator"  (fieldToJSON v)
  jsonField (MibCaptureTimeSemantics v)               = singleton "mibCaptureTimeSemantics"  (fieldToJSON v)
  jsonField (MibContextEngineID v)                    = singleton "mibContextEngineID"  (fieldToJSON v)
  jsonField (MibContextName v)                        = singleton "mibContextName"  (fieldToJSON v)
  jsonField (MibObjectName v)                         = singleton "mibObjectName"  (fieldToJSON v)
  jsonField (MibObjectDescription v)                  = singleton "mibObjectDescription"  (fieldToJSON v)
  jsonField (MibObjectSyntax v)                       = singleton "mibObjectSyntax"  (fieldToJSON v)
  jsonField (MibModuleName v)                         = singleton "mibModuleName"  (fieldToJSON v)
  jsonField (MobileIMSI v)                            = singleton "mobileIMSI"  (fieldToJSON v)
  jsonField (MobileMSISDN v)                          = singleton "mobileMSISDN"  (fieldToJSON v)
  jsonField (HttpStatusCode v)                        = singleton "httpStatusCode"  (fieldToJSON v)
  jsonField (OtherField code v) = singleton ("field" <> fromString (show code)) (fieldToJSON v)


  class FieldToJSON a where
    fieldToJSON :: a -> Value

  instance FieldToJSON Bool where
    fieldToJSON = toJSON

  instance FieldToJSON Word where
    fieldToJSON = toJSON

  instance FieldToJSON Word64 where
    fieldToJSON = toJSON

  instance FieldToJSON Word32 where
    fieldToJSON = toJSON

  instance FieldToJSON Word16 where
    fieldToJSON = toJSON

  instance FieldToJSON Word8 where
    fieldToJSON = toJSON

  instance FieldToJSON Int64 where
    fieldToJSON = toJSON

  instance FieldToJSON Double where
    fieldToJSON = toJSON

  instance FieldToJSON Text where
    fieldToJSON = toJSON

  instance FieldToJSON BS.ByteString where
    fieldToJSON = toJSON . decodeUtf8 . Base64.encode

  instance FieldToJSON MAC where
    fieldToJSON = toJSON . show

  instance FieldToJSON IPv4 where
    fieldToJSON = toJSON . show

  instance FieldToJSON IPv6 where
    fieldToJSON = toJSON . show


  class DecodeAs a where
    decodeAs :: Word16 -> (a -> Field) -> BS.ByteString -> Field


  instance DecodeAs BS.ByteString where
    decodeAs _fid f = f


  instance DecodeAs Bool where
    decodeAs fid f bs = case (roll bs :: Word) of
                         1 -> f True
                         2 -> f False
                         _ -> OtherField fid bs


  instance DecodeAs Word where
    decodeAs _fid f = f . roll


  instance DecodeAs Word8 where
    decodeAs _fid f = f . roll


  instance DecodeAs Word16 where
    decodeAs _fid f = f . roll


  instance DecodeAs Word32 where
    decodeAs _fid f = f . roll


  instance DecodeAs Word64 where
    decodeAs _fid f = f . roll


  instance DecodeAs Int64 where
    decodeAs fid f bs = case runGet getInt64be bs of
                          Left _e -> OtherField fid bs
                          Right v -> f v


  instance DecodeAs T.Text where
    decodeAs fid f bs = case decodeUtf8' bs of
                          Left _e -> OtherField fid bs
                          Right v -> f v


  instance DecodeAs Double where
    decodeAs fid f bs = case runGet getFloat64be bs of
                          Left _e -> OtherField fid bs
                          Right v -> f v


  instance DecodeAs IPv4 where
    decodeAs fid f bs = case BS.length bs of
                          4 -> f (toIPv4 $ map fromIntegral $ BS.unpack bs)
                          _ -> OtherField fid bs


  instance DecodeAs IPv6 where
    decodeAs fid f bs = case BS.length bs of
                          16 -> f (toIPv6b $ map fromIntegral $ BS.unpack bs)
                          _x -> OtherField fid bs


  instance DecodeAs MAC where
    decodeAs fid f bs = case BS.length bs of
                          6 -> f (toMAC $ BS.unpack bs)
                          _ -> OtherField fid bs


  roll :: (Integral a, Bits a) => BS.ByteString -> a
  roll = foldl' (\c n -> c `shiftL` 8 + n) 0 . map fromIntegral . BS.unpack


-- vim:set ft=haskell sw=2 ts=2 et:
