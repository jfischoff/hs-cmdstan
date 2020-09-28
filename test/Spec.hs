import CmdStan.SummaryParser
import CmdStan.Types
import Test.Hspec
import Control.Exception
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.String.Here.Uninterpolated
import qualified Data.Map.Strict as Map

testParser :: Parsec String String a -> String -> IO a
testParser parser input =
  either (throwIO . userError . show) pure $ parse parser "" input

main :: IO ()
main = hspec $ do
  describe "parseInputFiles" $ do
    it "works for a single file" $
      testParser parseInputFiles "Input file: output.csv" `shouldReturn` ["output.csv"]
    it "works for a two files" $
      testParser parseInputFiles "Input files: output1.csv, output2.csv" `shouldReturn`
        ["output1.csv", "output2.csv"]

  describe "parseInferenceModel" $ do
    it "works" $
      testParser parseInferenceModel "Inference for Stan model: bernoulli_model"
        `shouldReturn` "bernoulli_model"

  describe "parseCounts" $ do
    it "works with 1 count" $
        testParser parseCounts "(1000)" `shouldReturn` [1000]
    it "works with 2 counts" $
        testParser parseCounts "(1000, 2000)" `shouldReturn` [1000, 2000]

  describe "parseChainInfo" $ do
    it "works with 1 chain" $ do
      let input = "1 chains: each with iter=(1000); warmup=(0); thin=(1); 1000 iterations saved."

      testParser parseChainInfo input `shouldReturn` ChainInfo
        { chainCount      = 1
        , chainIterCounts = [1000]
        , warmupCounts    = [0]
        , thinCounts      = [1]
        , savedIterations = 1000
        }

    it "works with 2 chain" $ do
      let input = "2 chains: each with iter=(1000,2000); warmup=(0,1); thin=(1,2); 3000 iterations saved."

      testParser parseChainInfo input `shouldReturn` ChainInfo
        { chainCount      = 2
        , chainIterCounts = [1000, 2000]
        , warmupCounts    = [0, 1]
        , thinCounts      = [1, 2]
        , savedIterations = 3000
        }

  describe "parseTimes" $ do
    it "works with 1 count" $
        testParser parseTimes "0.0070" `shouldReturn` [0.0070]
    it "works with 2 counts" $
        testParser parseTimes "(0.0070, 0.0080)" `shouldReturn` [0.0070, 0.0080]

  describe "parseWarmupTimes" $ do
    it "works with 1 count" $
        testParser parseWarmupTimes "Warmup took 0.0070 seconds"
          `shouldReturn` [0.0070]
    it "works with 2 counts" $
        testParser parseWarmupTimes "Warmup took (0.0070, 0.0080) seconds, 0.014 seconds total"
          `shouldReturn` [0.0070, 0.0080]

  describe "parseSamplingTimes" $ do
    it "works with 1 count" $
        testParser parseSamplingTimes "Sampling took 0.017 seconds"
          `shouldReturn` [0.017]
    it "works with 2 counts" $
        testParser parseSamplingTimes "Sampling took (0.016, 0.017) seconds, 0.033 seconds total"
          `shouldReturn` [0.016, 0.017]

  describe "skipUntilLp" $
    it "works" $ do
      let input = [here|
Mean     MCSE  StdDev     5%   50%   95%    N_Eff  N_Eff/s    R_hat

lp__            -7.3  2.4e-02    0.71   -8.6  -7.0  -6.8      888    26907     1.00
      |]

      testParser (skipUntilLp >> string "lp__") input `shouldReturn` "lp__"

  describe "parseStanStatistic" $
    it "works" $ do
      let input = "lp__            -7.3  2.4e-02    0.71   -8.6  -7.0  -6.8      888    26907     1.00"
      testParser parseStanStatistic input `shouldReturn`
        ("lp__", StanStatistic
          { mean         = -7.3
          , mcse         = 2.4e-02
          , stdDev       = 0.71
          , percents     = [-8.6, -7.0, -6.8]
          , numberOfEff  = 888
          , effPerSecond = 26907
          , rHat         = 1.00
          }
        )

  describe "parseStatisticMap" $ it "works" $ do
      let input = "lp__            -7.3  2.4e-02    0.71   -8.6  -7.0  -6.8      888    26907     1.00\ntheta           0.25  5.8e-03    0.12  0.077  0.23  0.45      401    23611      1.0\n"

      testParser parseStatisticMap input `shouldReturn` Map.fromList
        [ ("lp__", StanStatistic
            { mean         = -7.3
            , mcse         = 2.4e-02
            , stdDev       = 0.71
            , percents     = [-8.6, -7.0, -6.8]
            , numberOfEff  = 888
            , effPerSecond = 26907
            , rHat         = 1.00
            }
          )
        , ("theta", StanStatistic
            { mean         = 0.25
            , mcse         = 5.8e-03
            , stdDev       = 0.12
            , percents     = [0.077,  0.23,  0.45]
            , numberOfEff  = 401
            , effPerSecond = 23611
            , rHat         = 1.00
            }
          )
        ]

  describe "parseSampler" $ it "works" $ do
    testParser parseSampler "Samples were drawn using hmc with nuts." `shouldReturn` "hmc with nuts"

  describe "parseAutcorreltations" $ it "works" $ do
    let input = [here|
Displaying the autocorrelations for chain 1:

  Lag          lp__ accept_stat__    stepsize__   treedepth__  n_leapfrog__   divergent__      energy__         theta
    0           1.0           1.0           1.0           1.0           1.0           nan           1.0           1.0
    1           0.4           0.0           1.0           0.0           0.1           nan           0.4           0.5
    |]

    fmap show (testParser parseAutcorreltations input) `shouldReturn` show (AutoCorrelations
      { chainIndex         = 1
      , lag                = [0, 1]
      , aLogProbability    = [1.0, 0.4]
      , aAcceptStat        = [1.0, 0.0]
      , aStepSize          = [1.0, 1.0]
      , aTreeDepth         = [1.0, 0.0]
      , aNumberOfLeapFrogs = [1.0, 0.1]
      , aDivergent         = [0.0/0, 0.0/0]
      , aEnergy            = [1.0, 0.4]
      , aParams            = Map.fromList [("theta", [1.0, 0.5])]
      })


  describe "parseStatisticMap" $ it "works" $ do
      let input = [here|
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
      |]

      show (parseStanSummary input) `shouldBe` show (Right StanSummary
          { sPercentiles     = [5,50,95]
          , inputFiles       = ["output1.csv", "output2.csv"]
          , inferenceModel   = "bernoulli_model"
          , chainInfo        = ChainInfo
            { chainCount      = 2
            , chainIterCounts = [1000, 1000]
            , warmupCounts    = [0, 0]
            , thinCounts      = [1, 1]
            , savedIterations = 2000
            }
          , warmupTimes      = [0.0070, 0.0070]
          , samplingTimes    = [0.016, 0.017]
          , logProbability   = StanStatistic
            { mean         = -7.3
            , mcse         = 2.4e-02
            , stdDev       = 0.71
            , percents     = [-8.6, -7.0, -6.8]
            , numberOfEff  = 888
            , effPerSecond = 26907
            , rHat         = 1.00
            }
          , acceptance       = StanStatistic
            { mean         = 0.92
            , mcse         = 2.8e-03
            , stdDev       = 0.12
            , percents     = [0.65,0.97,1.0]
            , numberOfEff  = 1900
            , effPerSecond = 58000.0
            , rHat         = 1.00
            }
          , stepSize         = StanStatistic
            { mean         =  0.94
            , mcse         = 2.5e-03
            , stdDev       = 0.080
            , percents     = [0.87,   1.0,   1.0]
            , numberOfEff  = 1.0e+03
            , effPerSecond = 3.0e+04
            , rHat         = 2.6e+13
            }
          , treeDepth        = StanStatistic
            { mean         = 1.4
            , mcse         = 1.1e-02
            , stdDev       = 0.48
            , percents     = [1.0, 1.0, 2.0]
            , numberOfEff  = 1.8e+03
            , effPerSecond = 5.4e+04
            , rHat         = 1.0e+00
            }
          , leapFrog         = StanStatistic
            { mean         = 2.5
            , mcse         = 3.0e-02
            , stdDev       = 1.3
            , percents     = [ 1.0, 3.0, 3.0]
            , numberOfEff  = 1700
            , effPerSecond = 5.2e+04
            , rHat         = 1.0e+00
            }
          , divergent        = StanStatistic
            { mean         = 0.00
            , mcse         = 0/0.0
            , stdDev       = 0.00
            , percents     = [0.00, 0.00, 0.00]
            , numberOfEff  = 0/0.0
            , effPerSecond = 0/0.0
            , rHat         = 0/0.0
            }
          , energy           = StanStatistic
            { mean         = 7.8
            , mcse         = 3.5e-02
            , stdDev       = 1.0
            , percents     = [6.8, 7.5, 9.8]
            , numberOfEff  = 8.6e+02
            , effPerSecond = 2.6e+04
            , rHat         = 1.0e+00
            }
          , paramStats       = Map.singleton "theta" $ StanStatistic
              { mean         = 0.25
              , mcse         = 4.6e-03
              , stdDev       = 0.12
              , percents     = [0.082, 0.23, 0.46]
              , numberOfEff  = 643
              , effPerSecond = 19477
              , rHat         = 1.00
              }

          , sampler          = "hmc with nuts"
          , autoCorrelations = Nothing
          , unparsed = input
          }
        :: Either String StanSummary)
