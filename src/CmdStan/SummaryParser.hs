module CmdStan.SummaryParser where
import CmdStan.Types
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad
import Control.Arrow
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Function

readMay :: Read a => String -> Maybe a
readMay = foldr (\(x, _) _ -> Just x) Nothing . reads

myFloat :: Parsec String String Double
myFloat = L.signed space (try L.float
                          <|> fmap fromIntegral (L.decimal :: Parsec String String Int)
                          <|> (string "nan" *> pure (0.0/0.0))
                          <|> (string "inf" *> pure (0.0/0.0))
                          <|> (string "+inf" *> pure (1.0/0.0))
                          <|> (string "-inf" *> pure (negate $ 1.0/0.0))
                         )

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

percentilesParser :: Parsec String String [Int]
percentilesParser = do
  (try (string "Percentiles: "  *> L.decimal `sepBy1` char ',')) <|> pure [5, 50, 95]

-- Input file: output.csv
parseSignificantDigits :: Parsec String String Int
parseSignificantDigits = do
  void $ string "Significant digits: "
  L.decimal
  
-- Input file: output.csv
parseInputFiles :: Parsec String String [FilePath]
parseInputFiles = do
  void $ string "Input file: " <|> string "Input files: "
  someTill (satisfy $ const True)
    (lookAhead $ void (char ',') <|> void (char '\n') <|> eof) `sepBy1` (char ',' *> space)

-- Input file: output.csv
parseOutputFile :: Parsec String String FilePath
parseOutputFile = do
  void $ string "Ouput csv_file: "
  someTill (satisfy $ const True)
    (lookAhead $ void (char '\n') <|> eof)


parseInferenceModel :: Parsec String String String
parseInferenceModel = do
  void $ string "Inference for Stan model: "
  some $ anySingleBut '\n'

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
  result <- someTill (satisfy $ const True) $ char '.'
  pure result

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
    _              <- optional parseSignificantDigits
    space
    outputFile     <- optional (parseOutputFile <?> "Parse Output Files")
    space
    sPercentiles   <- percentilesParser <?> "Parse Percentiles"
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
    let unparsed = input
    pure StanSummary {..}



-------------------------------------------------------------------------------
--- CSV Parser
-------------------------------------------------------------------------------

commaSeparated :: Parsec String String [String]
commaSeparated = do
  res <- someTill (satisfy $ const True)
    (lookAhead $ void (char ',') <|> void (char '\n') <|> eof)
      `sepBy1` (char ',' *> space)
  space
  pure res

-- [name,Mean,MCSE,StdDev,5%,50%,95%,N_Eff,N_Eff/s,R_hat]
toPercentiles :: [String] -> Parsec String String [Int]
toPercentiles
  = mapM (\x -> maybe (fail $ "failed to parse " <> x) pure . readMay $ init x)
  . reverse
  . drop 3
  . reverse
  . drop 4

toDoubleList :: [String] -> Parsec String String [Double]
toDoubleList xs = forM xs $ \a ->
  maybe (fail $ "failed to parse as Double " ++ show a) pure $ readMay a <|>
    (if a == "nan" then Just (0.0/0.0) else Nothing)

toStanStatistic :: [Double] -> Parsec String String StanStatistic
toStanStatistic xs = case xs of
  mean:mcse:stdDev:xs' -> case reverse xs' of
    rHat:effPerSecond:numberOfEff:xs'' -> do
      let percents = reverse xs''
      pure StanStatistic {..}
    _ -> fail $ "Could not turn Double list into StanStatistic with " ++ show xs
  _ -> fail $ "Could not turn Double list into StanStatistic with " ++ show xs

toNamedStanStatistic :: String -> [String] -> Parsec String String StanStatistic
toNamedStanStatistic name = \case
  [] -> fail "empty stan statistic"
  x:xs
    | x == name || x == ("\"" ++ name ++ "\"") -> toStanStatistic =<< toDoubleList xs
    | otherwise -> fail $ "expected name " ++ show name ++ " but got " ++ show x

toParamStanStatistic :: [String] -> Parsec String String (String, StanStatistic)
toParamStanStatistic = \case
  [] -> fail "empty stan statistic"
  x:xs -> fmap (x,) $ toStanStatistic =<< toDoubleList xs

summaryCsvParser :: String -> Either String StanSummary
summaryCsvParser input = left show $ parse theParser "" input where
  theParser = do
    sPercentiles   <- toPercentiles =<< commaSeparated
    logProbability <- toNamedStanStatistic "lp__" =<< commaSeparated
    acceptance     <- toNamedStanStatistic "accept_stat__" =<< commaSeparated
    stepSize       <- toNamedStanStatistic "stepsize__" =<< commaSeparated
    treeDepth      <- toNamedStanStatistic "treedepth__" =<< commaSeparated
    leapFrog       <- toNamedStanStatistic "n_leapfrog__" =<< commaSeparated
    divergent      <- toNamedStanStatistic "divergent__" =<< commaSeparated
    energy         <- toNamedStanStatistic "energy__" =<< commaSeparated
    paramStats     <- fmap Map.fromList $ manyTill (toParamStanStatistic =<< commaSeparated)
      (lookAhead $ void (char '#') <|> eof)
    inferenceModel <- char '#' *> space *> parseInferenceModel <* space
    chainInfo      <- char '#' *> space *> parseChainInfo <?> "Parse Chain Info"
    space
    void $ char '#' *> space
    warmupTimes    <- char '#' *> space *> parseWarmupTimes <?> "Parse Warmup Times"
    space
    samplingTimes  <- char '#' *> space *> parseSamplingTimes <?> "Parse Sampling Times"
    space
    sampler        <- char '#' *> space *> parseSampler <?> "Parse parameter stats"
    space

    -- TODO parse autocorrelations
    let
      inputFiles = []
      outputFile = Nothing
      autoCorrelations = Nothing
      unparsed = input

    pure StanSummary {..}
