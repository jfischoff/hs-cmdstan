module CmdStan where
import System.Process
import System.Exit
import Data.List
import GHC.Generics
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.FilePath.Posix
import Control.Exception hiding (try)
import Text.Megaparsec
import Text.Megaparsec.Char
import System.IO
import Control.Monad
import Control.Arrow
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Function

throwWhenExitFailure :: ExitCode -> IO ()
throwWhenExitFailure = \case
  ExitSuccess -> pure ()
  e@ExitFailure {} -> throwIO e

data CompilationConfig = CompilationConfig
  { modelName       :: FilePath
  , modelFilePath   :: FilePath
  , exeFilePath     :: FilePath
  , outputCppFile   :: FilePath
  , allowUndefined  :: Bool
  , includePaths    :: [FilePath]
  , useOpenCL       :: Bool
  , autoFormat      :: Bool
  , printCanocial   :: Bool
  , printCpp        :: Bool
  , allOptimization :: Bool
  , warnUnitialized :: Bool
  } deriving(Show, Eq, Ord, Generic)

makeDefaultCompilationConfig :: FilePath -> CompilationConfig
makeDefaultCompilationConfig modelFilePath = CompilationConfig
  { modelName       = takeBaseName modelFilePath
  , modelFilePath   = modelFilePath
  , exeFilePath     = dropExtension modelFilePath
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
  , "--name=" <> modelName
  , "--o=" <> outputCppFile
  , if allowUndefined then "--allow-undefined" else ""
  , "--include_paths=" <> intercalate "," includePaths
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

myFloat :: Parsec String String Double
myFloat = L.signed space (try L.float <|> fmap fromIntegral (L.decimal :: Parsec String String Int) <|> string "nan" *> pure (0.0/0.0))

{-
Input file: output.csv
Inference for Stan model: bernoulli_model
1 chains: each with iter=(1000); warmup=(0); thin=(1); 1000 iterations saved.

Warmup took 0.0070 seconds
Sampling took 0.017 seconds

                Mean     MCSE   StdDev     5%   50%   95%    N_Eff  N_Eff/s    R_hat

lp__            -7.2  3.7e-02     0.69   -8.8  -7.0  -6.7      348    20472      1.0
accept_stat__   0.93  3.3e-03  1.1e-01   0.69  0.98   1.0  1.0e+03  6.1e+04  1.0e+00
stepsize__      0.95      nan  1.8e-15   0.95  0.95  0.95      nan      nan      nan
treedepth__      1.4  1.5e-02  4.9e-01    1.0   1.0   2.0  1.1e+03  6.2e+04  1.0e+00
n_leapfrog__     2.7  5.1e-02  1.6e+00    1.0   3.0   7.0  9.8e+02  5.8e+04  1.0e+00
divergent__     0.00      nan  0.0e+00   0.00  0.00  0.00      nan      nan      nan
energy__         7.7  4.8e-02  9.8e-01    6.8   7.4   9.7  4.2e+02  2.5e+04  1.0e+00

theta           0.25  5.8e-03     0.12  0.077  0.23  0.45      401    23611      1.0

Samples were drawn using hmc with nuts.
For each parameter, N_Eff is a crude measure of effective sample size,
and R_hat is the potential scale reduction factor on split chains (at
convergence, R_hat=1).

-}

-- Input file: output.csv
parseInputFiles :: Parsec String String [FilePath]
parseInputFiles = do
  void $ string "Input file: " <|> string "Input files: "
  someTill (satisfy $ const True)
    (lookAhead $ void (char ',') <|> void (char '\n') <|> eof) `sepBy1` (char ',' *> space)

parseInferenceModel :: Parsec String String String
parseInferenceModel = do
  void $ string "Inference for Stan model: "
  some $ anySingleBut '\n'

data ChainInfo = ChainInfo
  { chainCount      :: Int
  , chainIterCounts :: [Int]
  , warmupCounts    :: [Int]
  , thinCounts      :: [Int]
  , savedIterations :: Int
  } deriving (Show, Eq, Ord, Generic)

{-
1 chains: each with iter=(1000); warmup=(0); thin=(1); 1000 iterations saved.

Warmup took 0.0070 seconds
Sampling took 0.017 seconds

2 chains: each with iter=(1000,1000); warmup=(0,0); thin=(1,1); 2000 iterations saved.

-}
parseCounts :: Parsec String String [Int]
parseCounts = do
  void $ char '('
  result <- L.decimal `sepBy1` (char ',' >> space)
  void $ char ')'
  pure result

parseChainInfo :: Parsec String String ChainInfo
parseChainInfo = do
  chainCount <- L.decimal
  void $ string " chains: each with"
  space
  chainIterCounts <- string "iter=" *> parseCounts
  void $ char ';'
  space
  warmupCounts    <- string "warmup=" *> parseCounts
  void $ char ';'
  space
  thinCounts      <- string "thin=" *> parseCounts
  void $ char ';'
  void space
  savedIterations <- L.decimal
  void space
  void $ string "iterations saved."
  pure ChainInfo {..}

parseTimes :: Parsec String String [Double]
parseTimes = fmap pure myFloat <|> do
  void $ char '('
  result <- myFloat `sepBy1` (char ',' >> space)
  void $ char ')'
  pure result
{-
Warmup took 0.0070 seconds
Sampling took 0.017 seconds

Warmup took (0.0070, 0.0070) seconds, 0.014 seconds total
Sampling took (0.016, 0.017) seconds, 0.033 seconds total
-}
parseWarmupTimes :: Parsec String String [Double]
parseWarmupTimes = do
  -- TODO skip spaces
  void $ string "Warmup took "
  result <- fmap pure myFloat <|> parseTimes
  void $ some $ anySingleBut '\n'
  pure result

parseSamplingTimes :: Parsec String String [Double]
parseSamplingTimes = do
  void $ string "Sampling took "
  result <- fmap pure myFloat <|> parseTimes
  void $ some $ anySingleBut '\n'
  pure result

skipUntilLp :: Parsec String String ()
skipUntilLp = void $ skipSomeTill (satisfy $ const True) $ lookAhead $ string "lp__"

data StanStatistic = StanStatistic
  { mean         :: Double
  , mcse         :: Double
  , stdDev       :: Double
  , percents     :: [Double]
  , numberOfEff  :: Double
  , effPerSecond :: Double
  , rHat         :: Double
  } deriving (Show, Eq, Ord, Generic)

{-
                Mean     MCSE  StdDev     5%   50%   95%    N_Eff  N_Eff/s    R_hat

lp__            -7.3  2.4e-02    0.71   -8.6  -7.0  -6.8      888    26907     1.00
-}
parseStanStatistic :: Parsec String String (String, StanStatistic)
parseStanStatistic = do
  name <- someTill (satisfy $ const True) (lookAhead spaceChar) <?> "Statistic Name"
  space
  mean <- myFloat <?> "mean"
  space
  mcse <- myFloat <?> "MCSE"
  space
  stdDev <- myFloat <?> "stddev"
  space
  rHat:effPerSecond:numberOfEff:rest <- reverse <$> myFloat `sepBy1` hspace1 <?> "Other stats"
  let percents = reverse rest
  pure (name, StanStatistic{..})

parseStatisticMap :: Parsec String String (Map String StanStatistic)
parseStatisticMap = fmap Map.fromList $ fix $ \next -> do
  (fmap Just (try $ parseStanStatistic <* newline) <|> pure Nothing) >>= \case
    Just x -> fmap (x :) next
    Nothing -> pure []

parseSampler :: Parsec String String String
parseSampler = do
  void $ string "Samples were drawn using "
  result <- string "hmc with nuts"
  void $ char '.'
  pure result

data AutoCorrelations = AutoCorrelations
  { chainIndex         :: Int
  , lag                :: [Double]
  , aLogProbability    :: [Double]
  , aAcceptStat        :: [Double]
  , aStepSize          :: [Double]
  , aTreeDepth         :: [Double]
  , aNumberOfLeapFrogs :: [Double]
  , aDivergent         :: [Double]
  , aEnergy            :: [Double]
  , aParams            :: Map String [Double]
  } deriving (Show, Eq, Ord)

{-
Displaying the autocorrelations for chain 1:

  Lag          lp__ accept_stat__    stepsize__   treedepth__  n_leapfrog__   divergent__      energy__         theta
    0           1.0           1.0           1.0           1.0           1.0           nan           1.0           1.0

-}

parseAutcorreltations :: Parsec String String AutoCorrelations
parseAutcorreltations = do
  chainIndex <- string "Displaying the autocorrelations for chain " *> L.decimal <* char ':'
  space
  "Lag":"lp__":"accept_stat__":"stepsize__":"treedepth__":"n_leapfrog__":"divergent__":"energy__":paramNames
    <- (someTill (satisfy $ const True) $ lookAhead spaceChar) `sepBy1` hspace1
  space
  lag:aLogProbability:aAcceptStat:aStepSize:aTreeDepth:aNumberOfLeapFrogs:aDivergent:aEnergy:paramValues <- transpose <$>
    (((hspace *> myFloat) `sepBy1` hspace1) `sepBy` newline)

  let aParams = Map.fromList $ zip paramNames paramValues

  pure AutoCorrelations {..}

data StanSummary = StanSummary
  { inputFiles       :: [FilePath]
  , inferenceModel   :: String
  , chainInfo        :: ChainInfo
  , warmupTimes      :: [Double]
  , samplingTimes    :: [Double]
  , logProbability   :: StanStatistic
  , acceptance       :: StanStatistic
  , stepSize         :: StanStatistic
  , treeDepth        :: StanStatistic
  , leapFrog         :: StanStatistic
  , divergent        :: StanStatistic
  , energy           :: StanStatistic
  , paramStats       :: Map String StanStatistic
  , sampler          :: String
  , autoCorrelations :: Maybe AutoCorrelations
  } deriving (Show, Eq, Ord, Generic)

{-
Input files: output1.csv, output2.csv
Inference for Stan model: bernoulli_model
2 chains: each with iter=(1000,1000); warmup=(0,0); thin=(1,1); 2000 iterations saved.

Warmup took (0.0070, 0.0070) seconds, 0.014 seconds total
Sampling took (0.016, 0.017) seconds, 0.033 seconds total

                Mean     MCSE  StdDev     5%   50%   95%    N_Eff  N_Eff/s    R_hat

lp__            -7.3  2.4e-02    0.71   -8.6  -7.0  -6.8      888    26907     1.00
accept_stat__   0.92  2.8e-03    0.12   0.65  0.97   1.0  1.9e+03  5.8e+04  1.0e+00
stepsize__      0.94  2.5e-03   0.080   0.87   1.0   1.0  1.0e+03  3.0e+04  2.6e+13
treedepth__      1.4  1.1e-02    0.48    1.0   1.0   2.0  1.8e+03  5.4e+04  1.0e+00
n_leapfrog__     2.5  3.0e-02     1.3    1.0   3.0   3.0  1.7e+03  5.2e+04  1.0e+00
divergent__     0.00      nan    0.00   0.00  0.00  0.00      nan      nan      nan
energy__         7.8  3.5e-02     1.0    6.8   7.5   9.8  8.6e+02  2.6e+04  1.0e+00

theta           0.25  4.6e-03    0.12  0.082  0.23  0.46      643    19477      1.0

Samples were drawn using hmc with nuts.
For each parameter, N_Eff is a crude measure of effective sample size,
and R_hat is the potential scale reduction factor on split chains (at
convergence, R_hat=1).

Displaying the autocorrelations for chain 1:

  Lag          lp__ accept_stat__    stepsize__   treedepth__  n_leapfrog__   divergent__      energy__         theta
    0           1.0           1.0           1.0           1.0           1.0           nan           1.0           1.0
    1           0.4          -0.0           1.0           0.0           0.1           nan           0.4           0.5
    2           0.2          -0.0           1.0          -0.0          -0.0           nan           0.2           0.2
    3           0.1          -0.0           1.0           0.0          -0.0           nan           0.1           0.1
    4           0.0          -0.0           1.0          -0.0          -0.0           nan           0.1           0.1
    5          -0.0          -0.0           1.0           0.0          -0.0           nan          -0.0           0.1
    6          -0.1          -0.0           1.0           0.0           0.0           nan          -0.1           0.1
    7          -0.1          -0.0           1.0          -0.1           0.0           nan          -0.1           0.1
    8          -0.0           0.0           1.0          -0.0           0.1           nan          -0.0           0.0
    9           0.0           0.1           1.0           0.0           0.0           nan          -0.0           0.0
   10          -0.0          -0.1           1.0          -0.0           0.0           nan          -0.0           0.0

-}

parseStanSummary :: String -> Either String StanSummary
parseStanSummary input = left show $ parse theParser "" input where
  theParser = do
    inputFiles     <- parseInputFiles <?> "Parse Input Files"
    space
    inferenceModel <- parseInferenceModel <?> "Parse Inference Model"
    space
    chainInfo      <- parseChainInfo <?> "Parse Chain Info"
    space
    warmupTimes    <- parseWarmupTimes <?> "Parse Warmup Times"
    space
    samplingTimes  <- parseSamplingTimes <?> "Parse Sampling Times"
    skipUntilLp <?> "Skip until lp__"
    ("lp__"         , logProbability) <- parseStanStatistic <* newline <?> "Parse lp__"
    ("accept_stat__", acceptance    ) <- parseStanStatistic <* newline <?> "Parse accept_stat__"
    ("stepsize__"   , stepSize      ) <- parseStanStatistic <* newline <?> "Parse stepsize__"
    ("treedepth__"  , treeDepth     ) <- parseStanStatistic <* newline <?> "Parse treedepth__"
    ("n_leapfrog__" , leapFrog      ) <- parseStanStatistic <* newline <?> "Parse n_leapfrog__"
    ("divergent__"  , divergent     ) <- parseStanStatistic <* newline <?> "Parse divergent__"
    ("energy__"     , energy        ) <- parseStanStatistic <* newline <?> "Parse energy__"
    space
    paramStats     <- parseStatisticMap <?> "Parse parameter stats"
    space
    sampler        <- parseSampler <?> "Parse parameter stats"
    space
    autoCorrelations <- optional parseAutcorreltations <?> "Parse autocorrelation"

    pure StanSummary {..}

data StansummaryConfig = StansummaryConfig
  { sampleFiles :: [FilePath]
  , autocorr    :: Maybe Int
  , csvFilePath :: Maybe FilePath
  , percentiles :: [Int]
  , sigFigs     :: Maybe Int
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

data Method
  = Sample { diagnosticFile :: Maybe FilePath }
  | Optimize
  | Variational { diagnosticFile :: Maybe FilePath }
  | GenerateQuantities
  | Diagnose

methodToCmdLine :: Method -> String
methodToCmdLine = \case
  Sample { .. } -> "sample" ++ maybe "" (" diagnostic_file=" <>) diagnosticFile
  Optimize -> "optimize"
  Variational { .. } ->  "variational" ++ maybe "" ("diagnostic_file=" <>) diagnosticFile
  GenerateQuantities -> "generate_quantities"
  Diagnose -> "diagnose"

data Initialization
  = IRealValue Double
  | IZero
  | IFilePath FilePath

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
