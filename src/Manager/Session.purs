module Manager.Session where

import Prelude

import Data.Api.Session (Session(..))
import Data.JSDate (getTime, now)
import Data.Map (Map, filter, insert, lookup)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.UUID (UUID, genUUID)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Effect.Aff.AVar (put, take)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Utils as Utils

type Sessions = Map UUID Session

startup :: Aff (AVar Sessions)
startup = AVar.new Map.empty

shutdown :: AVar Sessions -> Aff Unit
shutdown avar = void $ take avar

expireSessions :: AVar Sessions -> Aff Unit
expireSessions avar = do
  sessions <- take avar
  nowTimestamp <- liftEffect $ now <#> getTime
  put (filter (\(Session { lastTime }) -> nowTimestamp - lastTime < sessionTimeout) sessions) avar

sessionTimeout :: Number
sessionTimeout = 4.0 * 60.0 * 60.0 * 1000.0

verifySession :: AVar Sessions -> UUID -> Aff (Maybe Session)
verifySession avar authToken = do
  expireSessions avar
  nowTimestamp <- liftEffect $ now <#> getTime
  Utils.withAVar avar
    \sessions ->
      let sessionMaybe = lookup authToken sessions <#> updateLastTime nowTimestamp in
      pure case sessionMaybe of
        Nothing -> Tuple sessions Nothing
        Just session -> do
          Tuple (insert authToken session sessions) (Just session)

deleteSession :: AVar Sessions -> UUID -> Aff Unit
deleteSession avar authToken = do
  sessions <- AVar.take avar
  AVar.put (Map.delete authToken sessions) avar

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