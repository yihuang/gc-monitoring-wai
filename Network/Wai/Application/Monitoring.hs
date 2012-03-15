{-# LANGUAGE ExistentialQuantification, RecordWildCards, OverloadedStrings #-}
module Network.Wai.Application.Monitoring (monitorGC) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified GHC.Stats as Stats
import Network.HTTP.Types (status200)
import Network.Wai (Application, Response(ResponseBuilder))
import Blaze.ByteString.Builder (fromLazyByteString)
import Data.Aeson (ToJSON(toJSON), encode)

monitorGC :: Application
monitorGC _ = do
    stats <- liftIO Stats.getGCStats
    return $ ResponseBuilder
                status200
                [("Content-Type", "application/json")]
                $ fromLazyByteString $ encode $ partitionGCStats stats

-- copied shamelessly from ekg.
-- Existential wrapper used for OO-style polymorphism.
data Json = forall a. ToJSON a => Json a

instance ToJSON Json where
    toJSON (Json x) = toJSON x

partitionGCStats :: Stats.GCStats
                 -> ([(Text, Json)], [(Text, Json)])
partitionGCStats (Stats.GCStats {..}) = (counters, gauges)
  where
    counters =
        [ ("bytes_allocated"          , Json bytesAllocated)
        , ("num_gcs"                  , Json numGcs)
        , ("num_bytes_usage_samples"  , Json numByteUsageSamples)
        , ("cumulative_bytes_used"    , Json cumulativeBytesUsed)
        , ("bytes_copied"             , Json bytesCopied)
        , ("mutator_cpu_seconds"      , Json mutatorCpuSeconds)
        , ("mutator_wall_seconds"     , Json mutatorWallSeconds)
        , ("gc_cpu_seconds"           , Json gcCpuSeconds)
        , ("gc_wall_seconds"          , Json gcWallSeconds)
        , ("cpu_seconds"              , Json cpuSeconds)
        , ("wall_seconds"             , Json wallSeconds)
        ]
    gauges =
        [ ("max_bytes_used"           , Json maxBytesUsed)
        , ("current_bytes_used"       , Json currentBytesUsed)
        , ("current_bytes_slop"       , Json currentBytesSlop)
        , ("max_bytes_slop"           , Json maxBytesSlop)
        , ("peak_megabytes_allocated" , Json peakMegabytesAllocated)
        , ("par_avg_bytes_copied"     , Json parAvgBytesCopied)
        , ("par_max_bytes_copied"     , Json parMaxBytesCopied)
        ]

