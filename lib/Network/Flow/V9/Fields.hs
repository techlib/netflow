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

module Network.Flow.V9.Fields
( Field(..)
, decodeField
) where
  import BasePrelude

  import Data.MAC (MAC, toMAC)
  import Data.IP (IPv4, IPv6, toIPv4, toIPv6b)
  import Data.Text.Encoding (decodeUtf8')
  import Data.Serialize.Get
  import Data.Serialize.IEEE754

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
    | InterfaceName String
    | InterfaceDescription String
    | SamplerName String
    | OctetTotalCount Word64
    | PacketTotalCount Word64
    | FlagsAndSamplerId Word32
    | FragmentOffset Word16
    | ForwardingStatus Word32
    | MplsVpnRouteDistinguisher BS.ByteString
    | MplsTopLabelPrefixLength Word8
    | SrcTrafficIndex Word32
    | DstTrafficIndex Word32
    | ApplicationDescription String
    | ApplicationId BS.ByteString
    | ApplicationName String
    | PostIpDiffServCodePoint Word8
    | MulticastReplicationFactor Word32
    | ClassName String
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
    | WlanSSID String
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
    | VRFname String
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
    | MetroEvcId String
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
    | NatPoolName String
    | AnonymizationFlags Word16
    | AnonymizationTechnique Word16
    | InformationElementIndex Word16
    | P2pTechnology String
    | TunnelTechnology String
    | EncryptedTechnology String
    | BgpValidityState Word8
    | IPSecSPI Word32
    | GreKey Word32
    | NatType Word8
    | InitiatorPackets Word64
    | ResponderPackets Word64
    | ObservationDomainName String
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
    | SelectorName String
    | UpperCILimit Double
    | LowerCILimit Double
    | ConfidenceLevel Double
    | InformationElementDataType Word8
    | InformationElementDescription String
    | InformationElementName String
    | InformationElementRangeBegin Word64
    | InformationElementRangeEnd Word64
    | InformationElementSemantics Word8
    | InformationElementUnits Word16
    | PrivateEnterpriseNumber Word32
    | VirtualStationInterfaceId BS.ByteString
    | VirtualStationInterfaceName String
    | VirtualStationUUID BS.ByteString
    | VirtualStationName String
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
    | UserName String
    | ApplicationCategoryName String
    | ApplicationSubCategoryName String
    | ApplicationGroupName String
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
    | MibContextName String
    | MibObjectName String
    | MibObjectDescription String
    | MibObjectSyntax String
    | MibModuleName String
    | MobileIMSI String
    | MobileMSISDN String
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


  instance DecodeAs String where
    decodeAs fid f bs = case decodeUtf8' bs of
                          Left _e -> OtherField fid bs
                          Right v -> f (T.unpack v)


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
