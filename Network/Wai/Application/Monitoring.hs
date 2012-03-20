{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Network.Wai.Application.Monitoring
  ( monitorGC
  , monitorGCResponse
  , encodeGCStats
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as M
import qualified GHC.Stats as Stats
import Network.HTTP.Types (status200)
import Network.Wai (Application, Response(ResponseBuilder))
import Blaze.ByteString.Builder (fromLazyByteString)
import Data.Aeson (toJSON, Value(Object))
import Data.Aeson.Encode (encode)

monitorGCResponse :: ResourceT IO Response
monitorGCResponse = do
    stats <- liftIO Stats.getGCStats
    return $ ResponseBuilder
                status200
                [("Content-Type", "application/json")]
                $ fromLazyByteString $ encode $ encodeGCStats stats

monitorGC :: Application
monitorGC = const monitorGCResponse

encodeGCStats :: Stats.GCStats
              -> Value
encodeGCStats (Stats.GCStats {..}) =
    Object $ M.fromList [("counters", counters), ("gauges", gauges)]
  where
    counters = Object $ M.fromList
        [ ("bytes_allocated"          , toJSON bytesAllocated)
        , ("num_gcs"                  , toJSON numGcs)
        , ("num_bytes_usage_samples"  , toJSON numByteUsageSamples)
        , ("cumulative_bytes_used"    , toJSON cumulativeBytesUsed)
        , ("bytes_copied"             , toJSON bytesCopied)
        , ("mutator_cpu_seconds"      , toJSON mutatorCpuSeconds)
        , ("mutator_wall_seconds"     , toJSON mutatorWallSeconds)
        , ("gc_cpu_seconds"           , toJSON gcCpuSeconds)
        , ("gc_wall_seconds"          , toJSON gcWallSeconds)
        , ("cpu_seconds"              , toJSON cpuSeconds)
        , ("wall_seconds"             , toJSON wallSeconds)
        ]
    gauges = Object $ M.fromList
        [ ("max_bytes_used"           , toJSON maxBytesUsed)
        , ("current_bytes_used"       , toJSON currentBytesUsed)
        , ("current_bytes_slop"       , toJSON currentBytesSlop)
        , ("max_bytes_slop"           , toJSON maxBytesSlop)
        , ("peak_megabytes_allocated" , toJSON $ peakMegabytesAllocated*1024*1024)
        , ("par_avg_bytes_copied"     , toJSON parAvgBytesCopied)
        , ("par_max_bytes_copied"     , toJSON parMaxBytesCopied)
        ]

