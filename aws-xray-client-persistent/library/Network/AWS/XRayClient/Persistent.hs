module Network.AWS.XRayClient.Persistent
  ( xraySqlBackend
  ) where

import Prelude

import Conduit
import Control.Lens
import Control.Monad (void)
import Data.Acquire (Acquire)
import Data.Foldable (for_)
import Data.IORef
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Time.Clock.POSIX
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sql.Types.Internal (IsPersistBackend(mkPersistBackend), SqlBackend(..))
import Network.AWS.XRayClient.Segment
import Network.AWS.XRayClient.TraceId
import System.Random
import System.Random.XRayCustom

-- | Modify a SqlBackend to send trace data to X-Ray.
--
-- >>> runSqlConn sql (xraySqlBackend "my-query" sendToDaemon backend)
xraySqlBackend
  :: (IsPersistBackend backend, BaseBackend backend ~ SqlBackend)
  => (XRaySegment -> IO ())
  -> IORef StdGen
  -> Text
  -> backend
  -> IO backend
xraySqlBackend sendTrace stdGenIORef subsegmentName =
  fmap mkPersistBackend . modifyBackend . persistBackend
 where
  modifyBackend backend = do
    -- N.B. by default persistent caches a Map Text Statement for each
    -- SqlBackend, where Text is a SQL query. When we wrap a backend to run it
    -- with XRay, we have to modify each Statement to record query timing. If
    -- backends are long-lived, then this poses a problem because we will
    -- continually wrap the same Statement. Therefore, we clear this cache each
    -- time we want to monitor things with XRay.
    newConnStmtMap <- newIORef Map.empty
    pure backend
      { connPrepare = connPrepare' (connPrepare backend)
      , connBegin = binaryTimerWrapper "BEGIN" (connBegin backend)
      , connCommit = unaryTimerWrapper "COMMIT" (connCommit backend)
      , connRollback = unaryTimerWrapper "ROLLBACK" (connRollback backend)
      , connStmtMap = newConnStmtMap
      }

  connPrepare' baseConnPrepare sql = do
    -- Create an IORef to store the start time. This is populated when a query
    -- begins in 'stmtQuery', and is then used in stmtReset to compute the
    -- total time.
    startTimeIORef <- newIORef Nothing

    statement <- baseConnPrepare sql
    pure statement
      { stmtQuery = stmtQuery' statement startTimeIORef
      , stmtReset = stmtReset' statement startTimeIORef sql
      }

  stmtQuery'
    :: forall m
     . MonadIO m
    => Statement
    -> IORef (Maybe POSIXTime)
    -> [PersistValue]
    -> Acquire (ConduitT () [PersistValue] m ())
  stmtQuery' statement startTimeIORef vals = do
    -- Record start time in IORef
    liftIO $ getPOSIXTime >>= writeIORef startTimeIORef . Just

    -- Create the Source and return it
    stmtQuery statement vals

  stmtReset' :: Statement -> IORef (Maybe POSIXTime) -> Text -> IO ()
  stmtReset' statement startTimeIORef sql = do
    stmtReset statement

    -- If start time exists (it should) then send the trace
    mStartTime <- readIORef startTimeIORef
    for_ mStartTime $ \startTime ->
      sendQueryTrace sendTrace subsegmentName startTime stdGenIORef sql

  unaryTimerWrapper sql action x = do
    startTime <- getPOSIXTime
    result <- action x
    sendQueryTrace sendTrace sql startTime stdGenIORef sql
    pure result

  binaryTimerWrapper sql action x y = do
    startTime <- getPOSIXTime
    result <- action x y
    sendQueryTrace sendTrace sql startTime stdGenIORef sql
    pure result

sendQueryTrace
  :: (XRaySegment -> IO ())
  -> Text
  -> POSIXTime
  -> IORef StdGen
  -> Text
  -> IO ()
sendQueryTrace sendTrace subsegmentName startTime stdGenIORef sql = do
  -- Record end time
  endTime <- getPOSIXTime

  -- Generate trace and send it off
  segmentId <- withRandomGenIORef stdGenIORef generateXRaySegmentId
  void
    $ sendTrace
    $ xraySubsegment subsegmentName segmentId startTime (Just endTime)
    & xraySegmentSql
    ?~ (xraySegmentSqlDef & xraySegmentSqlSanitizedQuery ?~ sql)
