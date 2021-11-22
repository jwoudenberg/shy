module Shy (main) where

import qualified Brick
import qualified Brick.BChan
import qualified Brick.Widgets.Edit as Edit
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import qualified Data.IORef
import qualified Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Encoding
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Input.Events as VtyEvents
import qualified System.Exit
import qualified System.IO.Error
import qualified System.Posix.Types
import qualified System.Process.Typed as Process

main :: IO ()
main = do
  initialVty <- mkVty
  chan <- Brick.BChan.newBChan 1
  endState <- Brick.customMain initialVty mkVty (Just chan) (app chan) initialState
  let finalCmd = Text.concat (Edit.getEditContents (editor endState))
  liftIO $ Data.Text.IO.putStrLn finalCmd

data State = State
  { editor :: Edit.Editor Text.Text Name,
    cmdProcess :: Maybe (Async.Async (), Data.IORef.IORef Builder.Builder),
    output :: Text.Text
  }

data Event = ProcessProducedNewOutput

data Name = Editor
  deriving (Eq, Ord, Show)

mkVty :: IO Vty.Vty
mkVty = Vty.mkVty Vty.defaultConfig {Vty.outputFd = Just stdError}

stdError :: System.Posix.Types.Fd
stdError = System.Posix.Types.Fd 2

app :: Brick.BChan.BChan Event -> Brick.App State Event Name
app chan =
  (Brick.simpleApp Brick.emptyWidget)
    { Brick.appDraw,
      Brick.appHandleEvent = appHandleEvent chan,
      Brick.appChooseCursor
    }

initialState :: State
initialState =
  State
    { editor = Edit.editor Editor (Just 1) "",
      cmdProcess = Nothing,
      output = ""
    }

appHandleEvent ::
  Brick.BChan.BChan Event ->
  State ->
  Brick.BrickEvent Name Event ->
  Brick.EventM Name (Brick.Next State)
appHandleEvent chan state event =
  case event of
    Brick.VtyEvent vtyEvent ->
      case vtyEvent of
        VtyEvents.EvKey VtyEvents.KEsc [] -> Brick.halt state
        VtyEvents.EvKey (VtyEvents.KChar 'c') [VtyEvents.MCtrl] -> Brick.halt state
        VtyEvents.EvKey VtyEvents.KEnter [] -> Brick.halt state
        _ -> do
          newEditor <- Edit.handleEditorEvent vtyEvent (editor state)
          newRef <- liftIO (Data.IORef.newIORef "")
          newCmdProcess <-
            liftIO . Async.async $ do
              case cmdProcess state of
                Nothing -> pure ()
                Just (async, _) -> Async.cancel async
              runCommand chan newRef (Text.concat (Edit.getEditContents newEditor))
          Brick.continue
            state
              { editor = newEditor,
                cmdProcess = Just (newCmdProcess, newRef),
                output = ""
              }
    Brick.AppEvent ProcessProducedNewOutput -> do
      newOutput <-
        case cmdProcess state of
          Nothing -> pure ""
          Just (_, ref) -> liftIO (Data.IORef.readIORef ref)
      Brick.continue
        state
          { output =
              Builder.toLazyText newOutput
                & Data.Text.Lazy.toStrict
          }
    Brick.MouseDown _ _ _ _ -> Brick.continue state
    Brick.MouseUp _ _ _ -> Brick.continue state

appDraw :: State -> [Brick.Widget Name]
appDraw state =
  [ Brick.vBox
      [ Brick.hBox
          [ Brick.txt "> ",
            Edit.renderEditor
              (Brick.txt . Text.concat)
              True
              (editor state)
          ],
        Brick.txtWrap (output state)
      ]
  ]

appChooseCursor :: State -> [Brick.CursorLocation Name] -> Maybe (Brick.CursorLocation Name)
appChooseCursor _ = Data.Maybe.listToMaybe

runCommand :: Brick.BChan.BChan Event -> Data.IORef.IORef Builder.Builder -> Text.Text -> IO ()
runCommand chan ref cmd =
  let stdin =
        Data.Text.Lazy.fromStrict cmd
          & Data.Text.Lazy.Encoding.encodeUtf8
          & Process.byteStringInput
      config =
        Process.proc "bash" []
          & Process.setStdin stdin
          & Process.setStdout Process.createPipe
          & Process.setStderr Process.createPipe
      writeLine line = do
        Data.IORef.atomicModifyIORef ref (\acc -> (acc <> line <> "\n", ()))
        _ <- Brick.BChan.writeBChanNonBlocking chan ProcessProducedNewOutput
        pure ()
      fromHandleToRef handle = do
        maybeStr <- Control.Exception.try $ Data.Text.IO.hGetLine handle
        case maybeStr of
          Left err ->
            if System.IO.Error.isEOFError err
              then pure ()
              else Control.Exception.throwIO err
          Right str -> do
            writeLine (Builder.fromText str)
            fromHandleToRef handle
   in Process.withProcessWait config $ \proc -> do
        Async.concurrently_
          (fromHandleToRef (Process.getStdout proc))
          (fromHandleToRef (Process.getStderr proc))
        exitCode <- Process.waitExitCode proc
        case exitCode of
          System.Exit.ExitSuccess -> pure ()
          System.Exit.ExitFailure code ->
            writeLine ("\n[failed with code: " <> Builder.fromString (show code) <> "]")
