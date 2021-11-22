module Shy (main) where

import qualified Brick
import qualified Brick.BChan
import qualified Brick.Widgets.Edit as Edit
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (for_)
import Data.Function ((&))
import qualified Data.IORef
import qualified Data.List
import qualified Data.Maybe
import qualified Data.String.AnsiEscapeCodes.Strip.Text
import qualified Data.Text as Text
import qualified Data.Text.IO
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Encoding
import qualified Data.Version
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.Input.Events as VtyEvents
import qualified Paths_shy
import qualified System.Directory as Directory
import qualified System.Environment
import qualified System.Exit
import System.FilePath ((</>))
import qualified System.IO.Error
import qualified System.Posix.Process
import qualified System.Posix.Types
import qualified System.Process.Typed as Process

main :: IO ()
main = do
  ownName <- System.Environment.getProgName
  args <- System.Environment.getArgs
  if ownName `elem` fakeBinaries
    then runFakeBin (ownName : args)
    else case args of
      [] -> run
      ["--help"] -> showHelp
      ["--version"] -> showVersion
      _ -> do
        showHelp
        System.Exit.exitFailure

showHelp :: IO ()
showHelp = do
  putStrLn "Usage:"
  putStrLn "  shy [options]"
  putStrLn ""
  putStrLn "OPTIONS"
  putStrLn "  --help       show this help text"
  putStrLn "  --version    show version of shy"

showVersion :: IO ()
showVersion =
  Data.Version.versionBranch Paths_shy.version
    & fmap show
    & Data.List.intercalate "."
    & putStrLn

-- | Binaries we don't want to run in shy because they modify files on the file
-- system.
fakeBinaries :: [String]
fakeBinaries = ["cp", "rm", "mv"]

runFakeBin :: [String] -> IO ()
runFakeBin args = do
  putStr "+ "
  for_ (Data.List.intersperse " " args) putStr
  putStr "\n"

run :: IO ()
run = do
  setupFakeBinaries
  initialVty <- mkVty
  chan <- Brick.BChan.newBChan 1
  shell <- Data.Maybe.fromMaybe Bash <$> detectShell
  endState <- Brick.customMain initialVty mkVty (Just chan) app (initialState chan shell)
  let finalCmd = Text.concat (Edit.getEditContents (editor endState))
  liftIO $ Data.Text.IO.putStrLn finalCmd

data State = State
  { editor :: Edit.Editor Text.Text Name,
    cmdProcess :: Maybe (Async.Async (), Data.IORef.IORef Builder.Builder),
    eventChannel :: Brick.BChan.BChan Event,
    shellToUse :: Shell,
    output :: Text.Text
  }

data Event = ProcessProducedNewOutput

data Name = Editor
  deriving (Eq, Ord, Show)

mkVty :: IO Vty.Vty
mkVty = Vty.mkVty Vty.defaultConfig {Vty.outputFd = Just stdError}

stdError :: System.Posix.Types.Fd
stdError = System.Posix.Types.Fd 2

app :: Brick.App State Event Name
app =
  (Brick.simpleApp Brick.emptyWidget)
    { Brick.appDraw,
      Brick.appHandleEvent,
      Brick.appChooseCursor
    }

initialState :: Brick.BChan.BChan Event -> Shell -> State
initialState eventChannel shellToUse =
  State
    { editor = Edit.editor Editor (Just 1) "",
      cmdProcess = Nothing,
      eventChannel,
      shellToUse,
      output = ""
    }

appHandleEvent ::
  State ->
  Brick.BrickEvent Name Event ->
  Brick.EventM Name (Brick.Next State)
appHandleEvent state event =
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
              runCommand
                (eventChannel state)
                (shellToUse state)
                newRef
                (Text.concat (Edit.getEditContents newEditor))
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
                & Data.String.AnsiEscapeCodes.Strip.Text.stripAnsiEscapeCodes
          }
    Brick.MouseDown _ _ _ _ -> Brick.continue state
    Brick.MouseUp _ _ _ -> Brick.continue state

appDraw :: State -> [Brick.Widget Name]
appDraw state =
  [ Brick.vBox
      [ Brick.hBox
          [ Brick.str (shellToString (shellToUse state)),
            Brick.txt "> ",
            Edit.renderEditor
              (Brick.txt . Text.concat)
              True
              (editor state)
          ],
        Brick.txt (output state)
      ]
  ]

appChooseCursor :: State -> [Brick.CursorLocation Name] -> Maybe (Brick.CursorLocation Name)
appChooseCursor _ = Data.Maybe.listToMaybe

runCommand ::
  Brick.BChan.BChan Event ->
  Shell ->
  Data.IORef.IORef Builder.Builder ->
  Text.Text ->
  IO ()
runCommand chan shell ref cmd =
  let stdin =
        Data.Text.Lazy.fromStrict cmd
          & Data.Text.Lazy.Encoding.encodeUtf8
          & Process.byteStringInput
      config =
        Process.proc (shellToString shell) []
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
   in do
        Process.withProcessWait config $ \proc -> do
          Async.concurrently_
            (fromHandleToRef (Process.getStdout proc))
            (fromHandleToRef (Process.getStderr proc))
          exitCode <- Process.waitExitCode proc
          case exitCode of
            System.Exit.ExitSuccess -> pure ()
            System.Exit.ExitFailure code ->
              writeLine ("\n[failed with code: " <> Builder.fromString (show code) <> "]")

setupFakeBinaries :: IO ()
setupFakeBinaries = do
  fakeBinariesDir <- Directory.getXdgDirectory Directory.XdgCache "shy"
  Directory.createDirectoryIfMissing True fakeBinariesDir
  self <- System.Environment.getExecutablePath
  for_ fakeBinaries $ \fakeBin -> do
    let path = fakeBinariesDir </> fakeBin
    Control.Exception.catch
      (Directory.removeFile path)
      ( \err ->
          if System.IO.Error.isDoesNotExistError err
            then pure ()
            else Control.Exception.throwIO err
      )
    Directory.createFileLink self path
  path <- System.Environment.getEnv "PATH"
  System.Environment.setEnv "PATH" (fakeBinariesDir <> ":" <> path)

data Shell = Bash | Fish | Zsh

shellToString :: Shell -> String
shellToString Bash = "bash"
shellToString Fish = "fish"
shellToString Zsh = "zsh"

detectShell :: IO (Maybe Shell)
detectShell = do
  parentId <- System.Posix.Process.getParentProcessID
  (exitCode, stdout) <-
    Process.proc "ps" ["-p", show parentId, "-o", "comm="]
      & Process.readProcessStdout
  let processName =
        Data.Text.Lazy.Encoding.decodeUtf8' stdout
          & either (\_ -> "") id
          & Data.Text.Lazy.strip

  pure $
    case (exitCode, processName) of
      (System.Exit.ExitFailure _, _) -> Nothing
      (System.Exit.ExitSuccess, "bash") -> Just Bash
      (System.Exit.ExitSuccess, "fish") -> Just Fish
      (System.Exit.ExitSuccess, "zsh") -> Just Zsh
      (_, _) -> Nothing
