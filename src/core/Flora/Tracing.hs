module Flora.Tracing
  ( TraceRunner
  , newTraceRunner
  , runTraceRunner
  ) where

import Data.List (isPrefixOf)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Effectful
import Effectful.Tracing
  ( Tracer
  , alwaysOn
  , defaultParentBasedConfig
  , parentBased
  )
import Effectful.Tracing.Interpreter.OpenTelemetry
  ( OtelConfig (..)
  , runTracerOTel
  )
import Effectful.Tracing.SpanLimits (defaultSpanLimits)
import Network.Socket (HostName)
import OpenTelemetry.Exporter.OTLP.Span qualified as OTLP
import OpenTelemetry.Processor.Batch.Span qualified as Batch

newtype TraceRunner
  = TraceRunner
      (forall es a. IOE :> es => Eff (Tracer : es) a -> Eff es a)

newTraceRunner
  :: Maybe HostName
  -> Text
  -> IO TraceRunner
newTraceRunner mEndpoint serviceName = do
  exporterConfig <- addEndpointOverride mEndpoint <$> OTLP.loadExporterEnvironmentVariables
  exporter <- OTLP.otlpExporter exporterConfig
  processor <- Batch.batchProcessor Batch.batchTimeoutConfig exporter
  let config =
        OtelConfig
          { spanProcessors = [processor]
          , instrumentationScope = fromString (Text.unpack serviceName)
          , sampler = parentBased (defaultParentBasedConfig alwaysOn)
          , spanLimits = defaultSpanLimits
          }
  pure $ TraceRunner (runTracerOTel config)

runTraceRunner :: IOE :> es => TraceRunner -> Eff (Tracer : es) a -> Eff es a
runTraceRunner (TraceRunner runTrace) = runTrace

addEndpointOverride :: Maybe HostName -> OTLP.OTLPExporterConfig -> OTLP.OTLPExporterConfig
addEndpointOverride Nothing config = config
addEndpointOverride (Just endpoint) config =
  config{OTLP.otlpEndpoint = Just (normaliseEndpoint endpoint)}

normaliseEndpoint :: HostName -> HostName
normaliseEndpoint endpoint
  | "http://" `isPrefixOf` endpoint = endpoint
  | "https://" `isPrefixOf` endpoint = endpoint
  | otherwise = "http://" <> endpoint
