module Shy (main) where

import qualified Brick
import qualified Brick.Widgets.Edit as Edit
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified Data.Void as Void
import qualified Graphics.Vty.Input.Events as VtyEvents

main :: IO ()
main = do
  endState <- Brick.defaultMain app initialState
  let finalCmd = Text.intercalate "\n" (Edit.getEditContents (editor endState))
  liftIO $ Data.Text.IO.putStrLn finalCmd

data State = State
  { editor :: Edit.Editor Text.Text Name
  }

type Event = Void.Void

data Name = Editor
  deriving (Eq, Ord, Show)

app :: Brick.App State Event Name
app = (Brick.simpleApp (Brick.txt "")) {Brick.appDraw, Brick.appHandleEvent}

initialState :: State
initialState =
  State
    { editor = Edit.editor Editor (Just 1) ""
    }

appHandleEvent :: State -> Brick.BrickEvent Name Event -> Brick.EventM Name (Brick.Next State)
appHandleEvent state event =
  case event of
    Brick.VtyEvent vtyEvent ->
      case vtyEvent of
        VtyEvents.EvKey VtyEvents.KEsc [] -> Brick.halt state
        VtyEvents.EvKey (VtyEvents.KChar 'c') [VtyEvents.MCtrl] -> Brick.halt state
        VtyEvents.EvKey VtyEvents.KEnter [] -> Brick.halt state
        _ -> do
          newEditor <- Edit.handleEditorEvent vtyEvent (editor state)
          Brick.continue state {editor = newEditor}
    Brick.AppEvent appEvent -> Void.absurd appEvent
    Brick.MouseDown _ _ _ _ -> Brick.continue state
    Brick.MouseUp _ _ _ -> Brick.continue state

appDraw :: State -> [Brick.Widget Name]
appDraw state =
  [ Edit.renderEditor
      (Brick.txt . Text.intercalate "\n")
      True
      (editor state)
  ]
