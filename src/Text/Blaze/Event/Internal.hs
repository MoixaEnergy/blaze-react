{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Blaze.Event.Internal
    ( EventHandler(..)
    , Event(..)
    , eventDataToJson
    , SomeEvent(..)
    , MouseButton(..)
    , MousePosition(..)
    , DomDelta(..)
    , DeltaValue(..)
    , File(..)
    ) where

import           Control.Applicative
import           Control.Monad

import           Data.Aeson                (ToJSON(..), FromJSON(..))
import qualified Data.Aeson                as Aeson
import qualified Data.Aeson.Types          as Aeson
import qualified Data.ByteString           as BS
import           Data.Monoid               ((<>))
import qualified Data.Text                 as T
import           Data.Time.Clock           (UTCTime)
import qualified Data.Vector               as V

import           Text.Blaze.Event.Keycode  (Keycode)
import           Text.Blaze.Event.Charcode (Charcode)

-- | An Event handler converting events to actions of type 'act'.
data EventHandler act = forall eventData. EventHandler (Event eventData) (eventData -> act)

instance Functor EventHandler where
    fmap f (EventHandler ev mkAct) = EventHandler ev (f . mkAct)

data SomeEvent = forall eventData. SomeEvent (Event eventData)

{-
Markup (EventHandler act) -> Markup (EventHandler act, Int)

Markup (EventHandler act) -> Markup (SomeEvent, Int)



instance ToJSON (Event eventData) where

-}

eventDataToJson :: Event eventData -> eventData -> Aeson.Value
eventDataToJson = error "eventDataToJson"


-- | One specific and incomplete specifications of event-handlers geared
-- towards their use with ReactJS.
data Event eventData where
    OnKeyDown  :: ![Keycode]  -> Event Keycode
    -- OnKeyUp    :: ![Keycode]  -> Event Keycode
    -- OnKeyPress :: ![Charcode] -> Event Keycode

    -- OnFocus    :: Event ()
    -- OnBlur     :: Event ()

    -- -- NOTE (asayers): In ReactJS, I believe OnInput has the same semantics as
    -- -- OnChange, so I won't bother adding it here.
    -- -- NOTE (asayers): Using the 'selected' attribute of <option> elements
    -- -- seems to be discouraged in ReactJS (doing this throws a warning).
    -- -- Therefore I'm removing OnSelectedChange in favour of using OnValueChange
    -- -- on the <select> element.
    -- OnValueChange   :: Event T.Text
    -- OnCheckedChange :: Event Bool
    -- OnSubmit        :: Event ()

    OnClick       :: ![MouseButton] -> Event MousePosition
    -- OnDoubleClick :: ![MouseButton] -> Event MousePosition
    -- OnMouseDown   :: ![MouseButton] -> Event MousePosition
    -- OnMouseUp     :: ![MouseButton] -> Event MousePosition
    -- OnMouseMove   ::                   Event MousePosition
    -- OnMouseEnter  ::                   Event MousePosition
    -- OnMouseLeave  ::                   Event MousePosition
    -- OnMouseOver   ::                   Event MousePosition
    -- OnMouseOut    ::                   Event MousePosition

    -- OnScroll :: Event Int
    -- OnWheel  :: Event DomDelta


    -- TODO (asayers): Implement these
    -- OnCopy  ([File] -> IO a)
    -- OnCut   ([File] -> IO a)
    -- OnPaste ([File] -> IO a)

    -- TODO (asayers): Implement these.
    -- OnDrag      ([File] -> IO a)
    -- OnDragEnd   ([File] -> IO a)
    -- OnDragEnter ([File] -> IO a)
    -- OnDragExit  ([File] -> IO a)
    -- OnDragLeave ([File] -> IO a)
    -- OnDragOver  ([File] -> IO a)
    -- OnDragStart ([File] -> IO a)
    -- OnDrop      ([File] -> IO a)

    -- NOTE (asayers): These events require special initialization in React,
    -- and aren't supported by jQuery, so I'll omit them for now.
    -- OnTouchCancel (IO a)
    -- OnTouchEnd    (IO a)
    -- OnTouchMove   (IO a)
    -- OnTouchStart  (IO a)


deriving instance Eq (Event eventData)
deriving instance Show (Event eventData)

instance ToJSON (Event eventData) where
    toJSON ev = case ev of
        OnKeyDown keys  -> tagged 0 [toJSON keys]
        OnClick buttons -> tagged 8 [toJSON buttons]
      where
        tagged :: Int -> [Aeson.Value] -> Aeson.Value
        tagged tag args = toJSON (toJSON tag : args)

instance ToJSON SomeEvent where
    toJSON (SomeEvent ev) = toJSON ev

instance FromJSON SomeEvent where
    parseJSON = Aeson.withArray "Event" $ \arr -> do
        tag <- maybe (fail "Expected tag.") parseJSON (arr V.!? 0)
        let pos :: FromJSON a => Int -> Maybe (Aeson.Parser a)
            pos i = parseJSON <$> (arr V.!? i)

            check :: Int -> Maybe (Aeson.Parser a) -> Aeson.Parser a
            check l mbM
              | l == V.length arr =
                  maybe (fail "Failed inner parse.") id mbM
              | otherwise     = fail $
                  "Expected length " <> show l <>
                  ", got " <> show (V.length arr) <> "."

            lift1 :: FromJSON a => (a -> b) -> Aeson.Parser b
            lift1 constr = check 2 (fmap constr <$> pos 1)

        case (tag :: Int) of
          0 -> lift1 (SomeEvent . OnKeyDown)
          8 -> lift1 (SomeEvent . OnClick)
          _ -> fail $ "Unexpected tag " <> show tag




data MouseButton
    = LeftButton
    | RightButton
    | MiddleButton
    deriving (Eq, Show, Enum, Bounded)

instance ToJSON MouseButton where
    toJSON = toJSON . fromEnum

instance FromJSON MouseButton where
    parseJSON = \val -> do
        i <- parseJSON val
        guard (lb <= i && i <= ub)
        return $ toEnum i
      where
        lb = fromEnum (minBound :: MouseButton)
        ub = fromEnum (maxBound :: MouseButton)



data MousePosition = MousePosition
    { mpClientX :: {-# UNPACK #-} !Int
      -- ^ x-position relative to the upper-left corner of the viewport
    , mpClientY :: {-# UNPACK #-} !Int
      -- ^ y-position relative to the upper-left corner of the viewport
    , mpPageX   :: {-# UNPACK #-} !Int
      -- ^ x-position relative to the upper-left corner of the content-area
    , mpPageY   :: {-# UNPACK #-} !Int
      -- ^ y-position relative to the upper-left corner of the content-area
    , mpScreenX :: {-# UNPACK #-} !Int
      -- ^ x-position relative to the upper-left corner of the physical screen
    , mpScreenY :: {-# UNPACK #-} !Int
      -- ^ y-position relative to the upper-left corner of the physical screen
    } deriving (Eq, Show)

data DomDelta
    = PixelDelta !DeltaValue
    | LineDelta  !DeltaValue
    | PageDelta  !DeltaValue

data DeltaValue = DeltaValue
    { deltaX :: {-# UNPACK #-} !Double
    , deltaY :: {-# UNPACK #-} !Double
    , deltaZ :: {-# UNPACK #-} !Double
    }

data File = File
    { fileName         ::                !T.Text
    , fileMimeType     ::                !T.Text
    , fileSize         :: {-# UNPACK #-} !Int
      -- ^ Size of the blob in bytes
    , fileLastModified ::                !UTCTime
    , fileRead         ::                !(IO BS.ByteString)
      -- ^ Read the contents of the blob.
      -- NOTE (SM): I'm not sure whether we can support this in a
      -- cross-platform way.
    }

