module App.Logging where

import Control.Monad.Log (Severity)
import Protolude

import App.Types

data LogMsg = LogMsg
  { level :: Severity
  , location :: Text
  , id :: Text
  , trackingId :: Text
  , service :: Text
  , vin :: Vincode
  , text :: Text
  }
{-
we need to be able to reproduce this
{"@timestamp":"2017-08-24T12:03:07.583Z","level":"INFO","host":"nbooktf","APP":"name-of-service","class":"Module.Name","ID":"number_of_log_statement_in_module","trackingID":"some_UUID","Service":"needs_to_match_REST_endpoint","vin":"set_by_caller_or_internal_context","data":"actual log message"}
-}
