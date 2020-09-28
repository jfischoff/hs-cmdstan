module CmdStan
  (
    -- * Compilation
    CompilationConfig (..)
  , makeDefaultCompilationConfig
  , compile
  -- * Stan
  , Method (..)
  , Initialization (..)
  , StanExeConfig (..)
  , makeDefaultSample
  , stan
  -- * Summary
  , ChainInfo (..)
  , StanStatistic (..)
  , AutoCorrelations (..)
  , StanSummary (..)
  , StansummaryConfig (..)
  , makeDefaultSummaryConfig
  , stansummary
  -- * diagnose
  , diagnose
  ) where
import System.Process
import System.Exit
import Data.List
import System.FilePath.Posix
import Control.Exception
import System.IO
import Control.Monad
import CmdStan.SummaryParser
import CmdStan.Types

throwWhenExitFailure :: ExitCode -> IO ()
throwWhenExitFailure = \case
  ExitSuccess -> pure ()
  e@ExitFailure {} -> throwIO e

makeDefaultCompilationConfig :: FilePath -> CompilationConfig
makeDefaultCompilationConfig modelFilePath = CompilationConfig
  { modelName       = Nothing
  , modelFilePath   = modelFilePath
  , outputCppFile   = dropExtension modelFilePath <.> "hpp"
  , allowUndefined  = False
  , includePaths    = []
  , useOpenCL       = False
  , autoFormat      = False
  , printCanocial   = False
  , printCpp        = False
  , allOptimization = False
  , warnUnitialized = False
  }

compilationConfigToCmdLine :: CompilationConfig -> FilePath
compilationConfigToCmdLine CompilationConfig {..} = unwords
  [ "stanc"
  , maybe "" ("--name=" <>) modelName
  , "--o=" <> outputCppFile
  , if allowUndefined then "--allow-undefined" else ""
  , "--include-paths=" <> intercalate "," includePaths
  , if useOpenCL then "--use-opencl" else ""
  , if autoFormat then "--auto-format" else ""
  , if printCanocial then "--print-canonical" else ""
  , if printCpp then "--print-cpp" else ""
  , if allOptimization then "--O" else ""
  , if warnUnitialized then "--warn-uninitialized" else ""
  , modelFilePath
  ]

compile :: CompilationConfig -> IO ()
compile = throwWhenExitFailure <=< system . compilationConfigToCmdLine

makeDefaultSummaryConfig :: [FilePath] -> StansummaryConfig
makeDefaultSummaryConfig files = StansummaryConfig
  { sampleFiles = files
  , autocorr    = Nothing
  , csvFilePath = Nothing
  , percentiles = [5, 50, 95]
  , sigFigs     = Nothing
  }

stansummaryConfigToCmdLine :: StansummaryConfig -> [String]
stansummaryConfigToCmdLine StansummaryConfig {..} =
  [ maybe "" (\x -> "--autocorr=" <> show x) autocorr
  , maybe "" ("--csv_filename=" <>) csvFilePath
  , if null percentiles then "" else "--percentiles=" <> intercalate "," (map show percentiles)
  , maybe "" (\x -> "--sig_figs=" <> show x) sigFigs
  ] ++ sampleFiles

stansummary :: StansummaryConfig -> IO StanSummary
stansummary config = do
  (exitCode, output, err) <- readProcessWithExitCode "statsummary" (stansummaryConfigToCmdLine config) ""
  hPutStrLn stderr err
  case exitCode of
    ExitFailure {} -> throwIO exitCode
    ExitSuccess {} -> either (throwIO . userError) pure $ parseStanSummary output

diagnose :: [FilePath] -> IO ()
diagnose sampleFilePaths = throwWhenExitFailure =<< system ("diagnose " <> unwords sampleFilePaths)

methodToCmdLine :: Method -> String
methodToCmdLine = \case
  Sample { .. } -> "sample" ++ maybe "" (" diagnostic_file=" <>) diagnosticFile
  Optimize -> "optimize"
  Variational { .. } ->  "variational" ++ maybe "" ("diagnostic_file=" <>) diagnosticFile
  GenerateQuantities -> "generate_quantities"
  Diagnose -> "diagnose"

initializationToCmdLine :: Initialization -> String
initializationToCmdLine x = "init=" <> case x of
  IRealValue value -> show value
  IZero -> "0"
  IFilePath filePath -> filePath

data StanExeConfig = StanExeConfig
  { method          :: Method
  , inputData       :: Maybe FilePath
  , output          :: Maybe FilePath
  , initialValues   :: Maybe Initialization
  , randomSeed      :: Maybe Int
  , refreshInterval :: Maybe Int
  , processId       :: Maybe Int
  }

makeDefaultSample :: FilePath -> Int -> StanExeConfig
makeDefaultSample rootPath chainIndex = StanExeConfig
  { method          = Sample Nothing
  , inputData       = Just $ rootPath <.> "json"
  , output          = Just $ rootPath <> "_" <> show chainIndex <.> "csv"
  , initialValues   = Nothing
  , randomSeed      = Nothing
  , refreshInterval = Nothing
  , processId       = Just chainIndex
  }

toStanExeCmdLine :: StanExeConfig -> String
toStanExeCmdLine StanExeConfig {..} = unwords
  [ methodToCmdLine method
  , maybe "" ("data file=" <>) inputData
  , maybe "" ("output file=" <>) output
  , maybe "" initializationToCmdLine initialValues
  , maybe "" (\x -> "random=" <> show x) randomSeed
  , maybe "" (\x -> "refresh=" <> show x) refreshInterval
  , maybe "" (\x -> "id=" <> show x) processId
  ]

stan :: FilePath -> StanExeConfig -> IO ()
stan exeFilePath config =
  throwWhenExitFailure =<< system (exeFilePath <> " " <> toStanExeCmdLine config)
