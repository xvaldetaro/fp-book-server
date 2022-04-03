module Manager.Session where

import Prelude

import Data.Api.Session (Session(..))
import Data.JSDate (getTime, now)
import Data.Map (Map, filter, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUID, parseUUID)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar (put, take)
import Effect.Aff.AVar as AVar
import Effect.Aff.AVar as Avar
import Effect.Class (liftEffect)
import Undefined (undefined)

type Sessions = Map UUID Session

startup :: Aff (AVar Sessions)
startup = AVar.empty

shutdown :: AVar Sessions -> Aff Unit
shutdown avar = void $ take avar

expireSessions :: AVar Sessions -> Aff Unit
expireSessions avar = do
  sessions <- take avar
  nowTimestamp <- liftEffect $ now <#> getTime
  put (filter (\(Session { lastTime }) -> nowTimestamp - lastTime < sessionTimeout) sessions) avar

sessionTimeout :: Number
sessionTimeout = 4.0 * 60.0 * 60.0 * 1000.0

verifySession :: AVar Sessions -> String -> Aff (Maybe Session)
verifySession avar authTokenStr = do
  expireSessions avar
  sessions <- take avar
  nowTimestamp <- liftEffect $ now <#> getTime
  let sessionMaybe = parseUUID authTokenStr >>= flip lookup sessions <#> updateLastTime nowTimestamp
  let Tuple newSessions newSession =
        case sessionMaybe of
          Nothing -> Tuple sessions Nothing
          Just session@(Session { authToken }) -> do
            Tuple (insert authToken session sessions) (Just session)
  put newSessions avar
  pure newSession

updateLastTime :: Number -> Session -> Session
updateLastTime newTimestamp (Session session) = Session $ session { lastTime = newTimestamp }

createSession :: AVar Sessions -> String -> Aff UUID
createSession avar userName = do
  sessions <- take avar
  lastTime <- liftEffect $ now <#> getTime
  authToken <- liftEffect genUUID
  let session = Session { authToken, userName, lastTime }
  put (insert authToken session sessions) avar
  pure authToken

sat :: ∀ a. (a -> Boolean) -> a -> Maybe a
sat pred x = if pred x then Just x else Nothing