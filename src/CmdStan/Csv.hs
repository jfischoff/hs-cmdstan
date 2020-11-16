module CmdStan.Csv where
import qualified Data.ByteString as B
import Data.Csv.Incremental
import qualified Data.Csv as C
import System.IO
import Data.Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as H
import Control.Monad

parseNamedRecordAsDoubles :: C.NamedRecord -> C.Parser (Map String Double)
parseNamedRecordAsDoubles x = do
  let pairs = H.toList x
  parsedPairs <- forM pairs $ \(k, v) -> do
    d <- C.parseField v
    s <- C.parseField k
    pure (s, d)
  pure $ M.fromList parsedPairs

singleRowCsv :: FilePath -> IO (Either String (Map String Double))
singleRowCsv filePath
  = fmap (fmap snd) $ foldCsvWithHeader parseNamedRecordAsDoubles filePath mempty $ \acc xs -> pure $ foldr (<>) acc xs


{-
singleRowCsv :: FilePath -> IO (Either String (Map String Double))
singleRowCsv filePath = fmap (fmap snd) $ foldCsvWithHeader parseNamedRecordAsDoubles filePath mempty $ \acc xs -> case xs of
  [] -> throwIO $ userError "singleRowCsv called on zero row "
  [x] -> pure x
  x:xs
   | all M.null xs -> pure x
   | otherwise -> throwIO $ userError $ "singleRowCsv called with more than one row: " ++ show xs
-}

foldCsvWithHeader :: forall a r.
    (C.NamedRecord -> C.Parser a)
  -> FilePath
  -> r
  -> (r -> [a] -> IO r)
  -> IO (Either String (C.Header, r))
foldCsvWithHeader parameterParser outputFile initial foldRecords = withFile outputFile ReadMode $ \csvFile -> do
  let headerHandler :: HeaderParser (Parser a) -> IO (Either String (C.Header, r))
      headerHandler = \case
         FailH _ errMsg -> pure $ Left errMsg
         PartialH more -> headerHandler =<< feed more
         DoneH header parser -> bimap fst (header,) <$> loop initial parser

      loop :: r -> Parser a -> IO (Either (String, r) r)
      loop acc (Fail _ errMsg) = pure $ Left (errMsg, acc)
      loop acc (Many rs k)    = do
        case sequence rs of
          Left err -> pure $ Left (err, acc)
          Right rs' -> do
            as <- feed k
            newR <- foldRecords acc rs'
            loop newR as
      loop acc (Done rs) = do

        either (pure . Left . (,acc)) (fmap Right . foldRecords acc)
         $ sequence rs

      feed k = do
          isEof <- hIsEOF csvFile
          if isEof
              then return $ k B.empty
              else do
                bsLine <- B.hGetLine csvFile
                case B.uncons bsLine of
                  Just (w, _) ->
                    if w == 35 then
                      feed k
                    else
                      pure $ k $ B.snoc bsLine 10
                  Nothing -> pure $ k B.empty

  headerHandler (decodeByNameWithP parameterParser C.defaultDecodeOptions)

foldCsvSkipHeader
  :: forall a r.
    (C.Record -> C.Parser a)
  -> FilePath
  -> r
  -> (r -> [a] -> IO r)
  -> IO (Either (String, r) r)
foldCsvSkipHeader parameterParser outputFile initial foldRecords = withFile outputFile ReadMode $ \csvFile -> do
  let loop :: r -> Parser a -> IO (Either (String, r) r)
      loop acc (Fail _ errMsg) = pure $ Left (errMsg, acc)
      loop acc (Many rs k)    = do
        case sequence rs of
          Left err -> pure $ Left (err, acc)
          Right rs' -> do
            as <- feed k
            newR <- foldRecords acc rs'
            loop newR as
      loop acc (Done rs) = do

        either (pure . Left . (,acc)) (fmap Right . foldRecords acc)
         $ sequence rs

      feed k = do
          isEof <- hIsEOF csvFile
          if isEof
              then return $ k B.empty
              else do
                bsLine <- B.hGetLine csvFile
                case B.uncons bsLine of
                  Just (w, _) ->
                    if w == 35 then
                      feed k
                    else
                      pure $ k $ B.snoc bsLine 10
                  Nothing -> pure $ k B.empty

  loop initial (decodeWithP parameterParser C.defaultDecodeOptions HasHeader)
