module Shy (main) where

import qualified Brick
import qualified Brick.Widgets.Edit as Edit
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Void as Void
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Input.Events as VtyEvents
import qualified System.Posix.Types
import qualified System.Process.Typed as Process

main :: IO ()
main = do
  initialVty <- mkVty
  endState <- Brick.customMain initialVty mkVty Nothing app initialState
  let finalCmd = Text.intercalate "\n" (Edit.getEditContents (editor endState))
  liftIO $ Data.Text.IO.putStrLn finalCmd

data State = State
  { editor :: Edit.Editor Text.Text Name,
    output :: Text.Text
  }

type Event = Void.Void

data Name = Editor
  deriving (Eq, Ord, Show)

mkVty :: IO Vty.Vty
mkVty = Vty.mkVty Vty.defaultConfig {Vty.outputFd = Just stdError}

stdError :: System.Posix.Types.Fd
stdError = System.Posix.Types.Fd 2

app :: Brick.App State Event Name
app = (Brick.simpleApp Brick.emptyWidget) {Brick.appDraw, Brick.appHandleEvent}

initialState :: State
initialState =
  State
    { editor = Edit.editor Editor (Just 1) "",
      output = ""
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
          newOutput <- liftIO $ runCommand (Text.intercalate "\n" (Edit.getEditContents newEditor))
          Brick.continue state {editor = newEditor, output = newOutput}
    Brick.AppEvent appEvent -> Void.absurd appEvent
    Brick.MouseDown _ _ _ _ -> Brick.continue state
    Brick.MouseUp _ _ _ -> Brick.continue state

appDraw :: State -> [Brick.Widget Name]
appDraw state =
  [ Edit.renderEditor
      (Brick.txt . Text.intercalate "\n")
      True
      (editor state),
    Brick.txtWrap (output state)
  ]

runCommand :: Text.Text -> IO Text.Text
runCommand cmd = do
  (_, output) <-
    Process.proc "bash" []
      & Process.setStdin (Process.byteStringInput (Data.Text.Lazy.Encoding.encodeUtf8 (Data.Text.Lazy.fromStrict cmd)))
      & Process.readProcessInterleaved
  Data.Text.Lazy.Encoding.decodeUtf8' output
    & either (\_ -> "") id
    & Data.Text.Lazy.toStrict
    & pure
