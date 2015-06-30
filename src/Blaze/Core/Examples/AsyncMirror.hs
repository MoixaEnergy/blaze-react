{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | An app that allows to play the client for an arbitrary other app running
-- on the server.
module Blaze.Core.Examples.AsyncMirror
  ( mirror
  , renderMirror
  , serveMirror
  ) where

import           Control.Applicative
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, writeTVar)
import           Control.Lens                (preview, _Left)
import           Control.Monad
import           Control.Monad.STM           (STM, atomically)
import           Control.Monad.State

import           Data.Traversable
import           Data.Monoid
-- import qualified Data.Aeson       as Aeson

import           Blaze.Core
import qualified Text.Blaze.Event          as E
import qualified Text.Blaze.Event.Internal as EI
import qualified Text.Blaze.Html5          as H


import Prelude hiding (lookup)

type IORequest act = (act -> IO ()) -> IO ()

type RevId = Int

-- | A position in a traversable structure.
type Pos = Int

-- | Client state encapsulating server-state 'ss' and view 'v'.
type MirrorS = Maybe (RevId, H.Html (Pos, E.SomeEventSelector))

-- | Actions that happen in the mirror application.
data MirrorA
    = UpdateReflectionA !RevId !(H.Html (Pos, E.SomeEventSelector))
      -- ^ Update the state and view of the reflection that we are
      -- maintaining.
    | HandleEventA !RevId !Pos !E.SomeEvent
      -- ^ Handle the event that was triggered in our view, which is the
      -- rendered version of the state with the given revision-id.

data MirrorR
    = GetReflectionR !(Maybe RevId)
      -- ^ Request the next mirror-image, which must have a revision-id that
      -- is larger than the one that we are currently displaying.
    | HandleEventR !RevId !Pos !E.SomeEvent

renderMirror :: MirrorS -> H.Html (E.EventHandler MirrorA)
renderMirror Nothing              = mempty
renderMirror (Just (revId, html)) =
    toAction <$> html
  where
    toAction (pos, EI.SomeEventSelector sel) =
        EI.EventHandler sel $ \evData ->
            HandleEventA revId pos (EI.SomeEvent (EI.Event sel evData))


-- | Create an app for proxying the session with the given 'SessionId', which
-- is usually chosen randomly.
--
-- We assume that each request for a next server state will be answered with
-- either 'Nothing' or a new server-state.
--
-- TODO (SM): this is more like mirroring a session over an unreliable channel
-- => consider a rename.
mirror :: App MirrorS MirrorA [MirrorR]
mirror = App
    { appInitialState   = Nothing
    , appInitialRequest = [GetReflectionR Nothing]
    , appApplyAction    = \act -> runApplyActionM $ do
        case act of
          HandleEventA revId pos someEv ->
            submitRequest [HandleEventR revId pos someEv]
          UpdateReflectionA revId view -> do
            writeState (Just (revId, view))
            submitRequest [GetReflectionR (Just revId)]
    }

lookupById :: Traversable f => Int -> f a -> Maybe a
lookupById i t =
    preview _Left $ execStateT (traverse lookup t) 0
  where
    lookup x = do
        nextId <- get
        put (succ nextId)
        when (nextId == i) (lift (Left x))


traverseWithId :: Traversable f => (Int -> a -> b) -> f a -> f b
traverseWithId f t =
    evalState (traverse annotate t) 0
  where
    annotate x = do
        nextId <- get
        let !nextId' = succ nextId
        put nextId'
        return (f nextId x)


runMirrored
    :: forall st act.
       (st -> H.Html (E.EventHandler act))
    -> App st act (IORequest act)
    -> IO ()
runMirrored render app = do
    -- setup mirroring server
    (handleGetReflection, handleEvent) <- serveMirror render app
    let evalMirrorR applyAction req =
            -- We must fork here because the handlers might block.
            forkIO $ case req of
                GetReflectionR mbRevId -> do
                  (revId, html) <- handleGetReflection mbRevId
                  applyAction (UpdateReflectionA revId html)

                HandleEventR revId pos someEv ->
                  handleEvent revId pos someEv

    -- run blaze react on mirrored app
    runBlazeReact renderMirror
        ((\reqs applyAction -> mapM_ (evalMirrorR applyAction) reqs) <$> mirror)


runBlazeReact
    :: (st -> H.Html (E.EventHandler act))
    -> App st act (IORequest act)
    -> IO ()
runBlazeReact = error "runBlazeReactApp"


serveMirror
    :: forall st act.
       (st -> H.Html (E.EventHandler act))
    -> App st act (IORequest act)
    -> IO ( Maybe RevId -> IO (RevId, H.Html (Pos, E.SomeEventSelector))
          , RevId -> Pos -> E.SomeEvent -> IO ()
          )
serveMirror render app = do
    -- allocate state reference
    stVar <- newTVarIO (appInitialState app, 0)

    -- execute the initial request
    appInitialRequest app (applyActionIO stVar)

    -- return event handlers
    return (handleGetReflection stVar, handleHandleEvent stVar)
  where
    applyAction :: TVar (st, RevId) -> act -> STM (IORequest act)
    applyAction stVar act = do
        (st, revId) <- readTVar stVar
        let (!st', !req) = appApplyAction app act st
        writeTVar stVar (st', succ revId)
        return req

    applyActionIO :: TVar (st, RevId) -> act -> IO ()
    applyActionIO stVar act = do
        req <- atomically (applyAction stVar act)
        req (applyActionIO stVar)

    -- function to handle a 'HandleEvent' mirror-request
    handleHandleEvent stVar evRevId pos someEv = do
        req <- atomically $ do
            (st, revId) <- readTVar stVar
            case lookupById pos (render st) of
              Nothing -> return emptyRequest -- ignore event that we cannot locate
              Just (EI.EventHandler sel evDataToAct)
                | revId /= evRevId -> return emptyRequest -- event does not match revision-id
                | otherwise        -> do
                    case EI.someEventData someEv sel of
                      Nothing     -> return emptyRequest -- event selectors do not match
                      Just evData -> applyAction stVar (evDataToAct evData)
        -- execute resulting request
        req (applyActionIO stVar)
      where
        emptyRequest _applyActionIO = return ()

    -- function to handle a 'GetReflection' mirror-request
    handleGetReflection stVar mbClientRevId = atomically $ do
        (st, revId) <- readTVar stVar
        -- only return an update once the revision-id has changed
        guard (mbClientRevId /= Just revId)
        return (revId, traverseWithId adapt (render st))
      where
        adapt pos (EI.EventHandler sel _mkAct) = (pos, EI.SomeEventSelector sel)

