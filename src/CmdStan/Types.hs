module CmdStan.Types where
import GHC.Generics
import Data.Map.Strict (Map)

data MakeConfig = MakeConfig
  { stancFlags :: Maybe StancConfig
  , userHeader :: Maybe FilePath
  , jobCount   :: Maybe Int
  , cmdStanDir :: FilePath
  , rootPath   :: FilePath
  } deriving(Show, Eq, Ord, Generic)

data StancConfig = StancConfig
  { modelName       :: Maybe FilePath
  , modelFilePath   :: FilePath
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

data ChainInfo = ChainInfo
  { chainCount      :: Int
  , chainIterCounts :: [Int]
  , warmupCounts    :: [Int]
  , thinCounts      :: [Int]
  , savedIterations :: Int
  } deriving (Show, Eq, Ord, Generic)

data StanStatistic = StanStatistic
  { mean         :: Double
  , mcse         :: Double
  , stdDev       :: Double
  , percents     :: [Double]
  , numberOfEff  :: Double
  , effPerSecond :: Double
  , rHat         :: Double
  } deriving (Show, Eq, Ord, Generic)

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

data StanSummary = StanSummary
  { sPercentiles     :: [Int]
  , inputFiles       :: [FilePath]
  , outputFile       :: Maybe FilePath
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
  , unparsed         :: String
  } deriving (Show, Eq, Ord, Generic)

data StansummaryConfig = StansummaryConfig
  { sampleFiles :: [FilePath]
  , autocorr    :: Maybe Int
  , csvFilePath :: Maybe FilePath
  , percentiles :: [Int]
  , sigFigs     :: Maybe Int
  } deriving (Show, Eq, Ord, Generic)

data Method
  = Sample { diagnosticFile :: Maybe FilePath }
  | Optimize
  | Variational { diagnosticFile :: Maybe FilePath }
  | GenerateQuantities
  | Diagnose
  deriving (Show, Eq, Ord, Generic)

data Initialization
  = IRealValue Double
  | IZero
  | IFilePath FilePath
  deriving (Show, Eq, Ord, Generic)
