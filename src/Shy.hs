module Shy (main) where

import qualified Brick
import qualified Brick.Widgets.Edit as Edit
import Data.Text (Text)
import qualified Data.Void as Void

main :: IO ()
main = do
  _ <- Brick.defaultMain app initialState
  pure ()

data State = State
  { editor :: Edit.Editor Text Name
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
    Brick.VtyEvent vtyEvent -> do
      newEditor <- Edit.handleEditorEvent vtyEvent (editor state)
      Brick.continue state {editor = newEditor}
    Brick.AppEvent appEvent -> Void.absurd appEvent
    Brick.MouseDown _ _ _ _ -> Brick.continue state
    Brick.MouseUp _ _ _ -> Brick.continue state

appDraw :: State -> [Brick.Widget Name]
appDraw state =
  [ Edit.renderEditor
      ( \editorLines ->
          case editorLines of
            [] -> Brick.txt ""
            line : _ -> Brick.txt line
      )
      True
      (editor state)
  ]
