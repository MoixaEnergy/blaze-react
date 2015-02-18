{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Text.Blaze.Event.Internal
    ( EventHandler(..)

    , MouseButton(..)
    , MousePosition(..)
    , DomDelta(..)
    , DeltaValue(..)
    , File(..)
    ) where

import qualified Data.ByteString           as BS
import qualified Data.Text                 as T
import           Data.Time.Clock           (UTCTime)

import           Text.Blaze.Event.Keycode  (Keycode)
import           Text.Blaze.Event.Charcode (Charcode)

-- | One specific and incomplete specifications of event-handlers geared
-- towards their use with ReactJS.
data EventHandler eventData where
    OnKeyDown  :: ![Keycode]  -> EventHandler ()
    OnKeyUp    :: ![Keycode]  -> EventHandler ()
    OnKeyPress :: ![Charcode] -> EventHandler ()

    OnFocus    :: EventHandler ()
    OnBlur     :: EventHandler ()

    -- NOTE (asayers): In ReactJS, I believe OnInput has the same semantics as
    -- OnChange, so I won't bother adding it here.
    -- NOTE (asayers): Using the 'selected' attribute of <option> elements
    -- seems to be discouraged in ReactJS (doing this throws a warning).
    -- Therefore I'm removing OnSelectedChange in favour of using OnValueChange
    -- on the <select> element.
    OnValueChange   :: !T.Text -> EventHandler ()
    OnCheckedChange :: !Bool   -> EventHandler ()
    OnSubmit        ::            EventHandler ()

    OnClick       :: ![MouseButton] -> EventHandler MousePosition
    OnDoubleClick :: ![MouseButton] -> EventHandler MousePosition
    OnMouseDown   :: ![MouseButton] -> EventHandler MousePosition
    OnMouseUp     :: ![MouseButton] -> EventHandler MousePosition
    OnMouseMove   ::                   EventHandler MousePosition
    OnMouseEnter  ::                   EventHandler MousePosition
    OnMouseLeave  ::                   EventHandler MousePosition
    OnMouseOver   ::                   EventHandler MousePosition
    OnMouseOut    ::                   EventHandler MousePosition

    OnScroll :: EventHandler Int
    OnWheel  :: EventHandler DomDelta


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

data MouseButton
    = LeftButton
    | RightButton
    | MiddleButton
    deriving (Eq, Show)

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

