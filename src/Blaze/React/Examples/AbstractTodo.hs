{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-
  Runtime-independent formulation of the TodoMVC app.
 -}

module Blaze.React.Examples.Todo
    ( app
    ) where

import           Prelude hiding (div)

import           Blaze.React

import           Control.Applicative
import           Control.Lens
                 ( makeLenses, view, traverse, folded, set, ix
                 , to, _2, _Just, sumOf, (%=), (.=), preuse, use
                 )
import           Control.Monad
import           Control.Monad.Trans.Maybe        (MaybeT(..), runMaybeT)

import           Data.Foldable   (foldMap)
import           Data.Maybe      (fromMaybe)
import           Data.Monoid     ((<>), mempty)
import qualified Data.Text       as T
import           Data.Typeable   (Typeable)

import qualified Text.Blaze.Event                     as E
import qualified Text.Blaze.Event.Keycode             as Keycode
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as A


------------------------------------------------------------------------------
-- State representation
------------------------------------------------------------------------------

data TodoItem = TodoItem
    { _tdDone :: !Bool
    , _tdDesc :: !T.Text
    } deriving (Eq, Ord, Show)

-- TODO (AS): Use an IntMap so we can persist the identity of the items when
-- new ones are added.
type TodoItems = [TodoItem]

-- | What item is being edited and together with the new text value that it
-- should have.
type EditFocus = Maybe (Int, T.Text)

-- | The state of our todo list editing app.
data TodoState = TodoState
    { _tsNewItemDesc :: !T.Text
    , _tsEditFocus   :: !EditFocus
    , _tsItems       :: !TodoItems
    } deriving (Eq, Show)


-- lenses
---------

makeLenses ''TodoItem
makeLenses ''TodoState


------------------------------------------------------------------------------
-- State transitions
------------------------------------------------------------------------------


-- representation
-----------------

-- | Serializable representations of state transitions possible for a list of
-- todo items.
data TodoItemsAction
    = SetItemA    Int Bool
    | DeleteItemA Int
    | SetAllItemsA Bool
      -- ^ If there is one item that is not completed, then set all items to
      -- completed. Otherwise, set all items to incomplete.
    | ClearCompletedA
      -- ^ Remove all completed items.
    deriving (Eq, Ord, Show, Read, Typeable)

-- | Serializable representations of state transitions possible for our todo
-- item management app.
data TodoAction
    = TodoItemsActionA TodoItemsAction
    | CreateItemA
    | UpdateNewItemDescA T.Text
    | EditItemA Int
    | UpdateEditTextA T.Text
    | CommitAndStopEditingA
    deriving (Eq, Ord, Show, Read, Typeable)


-- execution
------------

applyTodoItemsAction :: TodoItemsAction -> TodoItems -> TodoItems
applyTodoItemsAction action items = case action of
    SetItemA itemIdx done -> set (ix itemIdx . tdDone) done items
    DeleteItemA itemIdx   -> map snd $ filter ((itemIdx /=) . fst) $ zip [0..] items
    SetAllItemsA done     -> set (traverse . tdDone) done items
    ClearCompletedA       -> filter (not . view tdDone) items

-- NOTE (meiersi): for production use we'd want to use a more expressive monad
-- that also logs reasons for exceptions. We probably also want to check
-- invariants at specific points to simplify formulating tests.
applyTodoAction :: TodoAction -> Transition TodoState TodoAction
applyTodoAction action =
    runTransitionM $ case action of
      TodoItemsActionA action' ->
          tsItems %= applyTodoItemsAction action'

      EditItemA itemIdx -> do
          commitAndStopEditing
          discardErrors $ do
              itemDesc <- MaybeT $ preuse (tsItems . ix itemIdx . tdDesc)
              tsEditFocus .= Just (itemIdx, itemDesc)

      CommitAndStopEditingA -> commitAndStopEditing

      UpdateEditTextA newText ->
          tsEditFocus . _Just . _2 .= newText

      CreateItemA -> do
          newItemDesc <- use tsNewItemDesc
          unless (T.null newItemDesc) $ do
              tsItems       %= (TodoItem False newItemDesc :)
              tsNewItemDesc .= ""

      UpdateNewItemDescA newText ->
          tsNewItemDesc .= newText
  where
    discardErrors :: Functor m => MaybeT m () -> m ()
    discardErrors m = fromMaybe () <$> runMaybeT m

    commitAndStopEditing :: TransitionM TodoState TodoAction
    commitAndStopEditing = discardErrors $ do
        (itemIdx, newDesc) <- MaybeT $ use tsEditFocus
        tsEditFocus                   .= Nothing
        tsItems . ix itemIdx . tdDesc .= newDesc


------------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------------

renderTodoState :: TodoState -> WindowState TodoAction
renderTodoState state = WindowState
    { _wsBody = renderBody state
    , _wsPath = ""
    }

renderBody :: TodoState -> H.Html TodoAction
renderBody (TodoState newItemDesc mbEditFocus items) = do
    -- app
    H.section H.! A.id "todoapp" $
      H.div $ do
        -- header
        H.header H.! A.id "header" $ do
          H.h1 "todos"
          H.input H.! A.id "new-todo"
                  H.! A.placeholder "What needs to be done?"
                  H.! A.autofocus True
                  H.! A.value (H.toValue newItemDesc)
                  H.! E.onValueChange UpdateNewItemDescA
                  H.! E.onKeyDown [Keycode.enter] CreateItemA

        -- items
        unless (null items) $ do
            H.section H.! A.id "main" $ do
              H.input
                  H.! A.type_ "checkbox"
                  H.! A.id "toggle-all"
                  H.! A.checked (numTodo == 0)
                  H.! E.onCheckedChange (TodoItemsActionA . SetAllItemsA)
              H.label H.! A.for "toggle-all" $ "Mark all as complete"
              H.ul H.! A.id "todo-list" $
                foldMap (renderTodoItem mbEditFocus) $ zip [0..] items

            -- item footer
            H.footer H.! A.id "footer" $ do
              H.span H.! A.id "todo-count" $ do
                H.strong (H.toHtml numTodo)
                (if numTodo == 1 then " item" else " items") <> " left"

              unless (numCompleted == 0) $
                H.button
                    H.! A.id "clear-completed"
                    H.! E.onClick' (TodoItemsActionA ClearCompletedA)
                    $ "Clear completed (" <> H.toHtml numCompleted <> ")"

    -- app footer
    H.footer H.! A.id "info" $ do
      H.p "Double-click to edit a todo"
      H.p $ do
        void $ "Created by "
        H.a H.! A.href "https://github.com/meiersi" $ "Simon Meier"
        void $ " based on the "
        H.a H.! A.href "https://github.com/facebook/flux" $ "flux"
        void $ " TodoMVC example by "
        H.a H.! A.href "http://facebook.com/bill.fisher.771" $ "Bill Fisher"

      H.p $ do
        void $ "A (future;-) part of "
        H.a H.! A.href "http://todomvc.com" $ "TodoMVC"
  where
    numTodo      = sumOf (folded . tdDone . to not . to fromEnum) items
    numCompleted = length items - numTodo


renderTodoItem :: EditFocus -> (Int, TodoItem) -> H.Html TodoAction
renderTodoItem mbEditFocus (itemIdx, TodoItem done desc) = do
   H.li H.! itemClass
        H.! A.key (H.toValue itemIdx)
     $ do H.div H.! A.class_ "view" $ do
            H.input
                H.! A.type_ "checkbox"
                H.! A.class_ "toggle"
                H.! A.checked done
                H.! E.onCheckedChange (TodoItemsActionA . SetItemA itemIdx)
            H.label
                H.! E.onDoubleClick' (EditItemA itemIdx)
                $ H.toHtml desc
            H.button
                H.! A.class_ "destroy"
                H.! E.onClick' (TodoItemsActionA (DeleteItemA itemIdx))
                $ mempty
          case mbEditFocus of
           Just (focusIdx, focusText)
               | focusIdx == itemIdx ->
                   H.input H.! A.class_ "edit"
                           H.! A.value (H.toValue focusText)
                           H.! A.autofocus True
                           H.! E.onValueChange UpdateEditTextA
                           H.! E.onBlur CommitAndStopEditingA
                           H.! E.onKeyDown [Keycode.enter] CommitAndStopEditingA
               | otherwise -> mempty
           Nothing         -> mempty
  where
    itemClass
      | isBeingEdited = A.class_ "editing"
      | done          = A.class_ "completed"
      | otherwise     = mempty

    isBeingEdited = Just itemIdx == fmap fst mbEditFocus


------------------------------------------------------------------------------
-- Defining and running the app
------------------------------------------------------------------------------

app :: App TodoState TodoAction
app = App
    { appInitialState    = initialState
    , appInitialRequests = []
    , appApplyAction     = applyTodoAction
    , appRender          = renderTodoState
    }

initialState :: TodoState
initialState = TodoState
    { _tsNewItemDesc = ""
    , _tsEditFocus = Nothing
    , _tsItems = []
    }


