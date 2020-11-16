module CmdStan
  (
  -- * make
    makeDefaultMakeConfig
  , make
  -- * stanc
  , StancConfig (..)
  , makeDefaultStancConfig
  , stanc
  -- * Stan
  , Method (..)
  , Initialization (..)
  , StanExeConfig (..)
  , blankSampleConfig
  , blankOptimizeConfig
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
  , summaryCsvParser
  -- * diagnose
  , diagnose
  , readOptimizeCsv
  , optimizeCsvToInitialValues
  ) where
import System.Process
import System.Exit
import Data.List
import System.FilePath.Posix
import Control.Exception
import System.IO
import Control.Monad
import CmdStan.Csv
import CmdStan.SummaryParser
import CmdStan.Types
import System.Directory
import System.Environment
import Data.Map.Strict (Map)
import Data.Function


throwWhenExitFailure :: ExitCode -> IO ()
throwWhenExitFailure = \case
  ExitSuccess -> pure ()
  e@ExitFailure {} -> throwIO e

makeDefaultMakeConfig :: FilePath -> IO MakeConfig
makeDefaultMakeConfig rootPath = do
  cmdStanDir <- maybe (throwIO $ userError "CMDSTAN_DIR not defined") pure =<<
    lookupEnv "CMDSTAN_DIR"

  canonicalPath <- canonicalizePath rootPath

  pure $ MakeConfig
    { stancFlags = Nothing
    , userHeader = Nothing
    , jobCount   = Nothing
    , rootPath   = canonicalPath
    , cmdStanDir = cmdStanDir
    }

makeConfigToCmdLine :: MakeConfig -> String
makeConfigToCmdLine MakeConfig {..} = unwords
  [ maybe "" ("USER_HEADER="<>) userHeader
  , maybe "" (\x -> "STANCFLAGS=\"" <> stancConfigToCmdLine x <> "\"") stancFlags
  , "make"
  , maybe "" (\x -> "-j" <> show x) jobCount
  , rootPath
  ]

make :: MakeConfig -> IO ()
make config@MakeConfig {..} = throwWhenExitFailure <=< withCurrentDirectory cmdStanDir $
  system $ makeConfigToCmdLine config

makeDefaultStancConfig :: FilePath -> StancConfig
makeDefaultStancConfig modelFilePath = StancConfig
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

stancConfigToCmdLine :: StancConfig -> FilePath
stancConfigToCmdLine StancConfig {..} = unwords
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

stanc :: StancConfig -> IO ()
stanc = throwWhenExitFailure <=< system . stancConfigToCmdLine

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
  filter (not . null)
  [ maybe "" (\x -> "--autocorr=" <> show x) autocorr
  , maybe "" ("--csv_filename=" <>) csvFilePath
  , if null percentiles then "" else "--percentiles=" <> intercalate "," (map show percentiles)
  , maybe "" (\x -> "--sig_figs=" <> show x) sigFigs
  ] ++ sampleFiles

stansummary :: StansummaryConfig -> IO (Either String StanSummary)
stansummary config = do
  (exitCode, output, err) <- readProcessWithExitCode "stansummary" (stansummaryConfigToCmdLine config) ""
  hPutStrLn stderr err
  case exitCode of
    ExitFailure {} -> throwIO exitCode
    ExitSuccess {} -> pure $ parseStanSummary output

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

blankOptimizeConfig :: StanExeConfig
blankOptimizeConfig = StanExeConfig
  { method          = Optimize
  , inputData       = Nothing
  , output          = Nothing
  , initialValues   = Nothing
  , randomSeed      = Nothing
  , refreshInterval = Nothing
  , processId       = Nothing
  , numSamples      = Nothing
  , algorithm       = Nothing
  }

blankSampleConfig :: StanExeConfig
blankSampleConfig = StanExeConfig
  { method          = Sample Nothing
  , inputData       = Nothing
  , output          = Nothing
  , initialValues   = Nothing
  , randomSeed      = Nothing
  , refreshInterval = Nothing
  , processId       = Nothing
  , numSamples      = Nothing
  , algorithm       = Nothing
  }

makeDefaultSample :: FilePath -> Int -> StanExeConfig
makeDefaultSample rootPath chainIndex = StanExeConfig
  { method          = Sample Nothing
  , inputData       = Just $ rootPath <.> "json"
  , output          = Just $ rootPath <> "_" <> show chainIndex <.> "csv"
  , initialValues   = Nothing
  , randomSeed      = Nothing
  , refreshInterval = Nothing
  , numSamples      = Nothing
  , processId       = Just chainIndex
  , algorithm       = Nothing
  }

toStanExeCmdLine :: StanExeConfig -> String
toStanExeCmdLine StanExeConfig {..} = unwords
  [ methodToCmdLine method
  , maybe "" (\x -> "algorithm=" <> show x) algorithm
  , maybe "" (\x -> "num_samples=" <> show x) numSamples
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

readOptimizeCsv :: FilePath -> IO (Either String (Map String Double))
readOptimizeCsv = singleRowCsv

skipHash :: Handle -> IO ()
skipHash theHandle = fix $ \next -> do
  -- no peek so I make my own
  originalPos <- hGetPosn theHandle
  currentChar <- hGetChar theHandle
  hSetPosn originalPos
  when (currentChar == '#') $ do
    _ <- hGetLine theHandle
    next

skipUntil :: Char -> Handle -> IO ()
skipUntil theChar theHandle = fix $ \next -> do
  currentChar <- hGetChar theHandle
  unless (currentChar == theChar) next

copyUntil :: Char -> Handle -> Handle -> IO ()
copyUntil theChar inputHandle outputHandle = fix $ \next -> do
  currentChar <- hGetChar inputHandle
  hPutChar outputHandle currentChar
  unless (currentChar == theChar) next

copyLineAfterComma :: Handle -> Handle -> IO ()
copyLineAfterComma inputFile outputFile = do
  skipUntil ',' inputFile
  copyUntil '\n' inputFile outputFile

-- Skip until not #
-- Skip until ','
-- copy the rest of the line
-- Skip until ','
-- copy the rest of the line
-- done
optimizeCsvToInitialValues :: FilePath -> FilePath -> IO ()
optimizeCsvToInitialValues inputCsv outputCsv = withFile outputCsv WriteMode $ \outputFile -> do
  withFile inputCsv ReadMode $ \inputFile -> do
    skipHash inputFile
    replicateM_ 2 $ copyLineAfterComma inputFile outputFile
