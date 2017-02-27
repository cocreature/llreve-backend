{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Llreve.Rise4Fun
  ( rise4funServer
  , Metadata(..)
  , Sample(..)
  , Tutorial(..)
  , Rise4funAPI
  , RunRequest(..)
  , RunResponse(..)
  , ToolOutput(..)
  ) where

import           Control.Applicative
import           Control.Concurrent.Sem
import           Control.Monad.Except
import           Data.Aeson hiding (Error)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Llreve.Run
import           Llreve.Solver
import           Llreve.Type
import           Servant
import           System.IO (hClose)
import           System.IO.Temp

data Sample = Sample
  { sampleName :: !Text
  , sampleSource :: !Text
  } deriving (Show, Eq, Ord)

instance ToJSON Sample where
  toJSON (Sample name source) = object ["Name" .= name, "Source" .= source]

data Tutorial = Tutorial
  { tutorialName :: !Text
  , tutorialSource :: !Text
  , tutorialSamples :: ![Sample]
  } deriving (Show, Eq, Ord)

instance ToJSON Tutorial where
  toJSON (Tutorial name source samples) =
    object ["Name" .= name, "Source" .= source, "Samples" .= samples]

data Metadata = Metadata
  { mdName :: !Text
  , mdDisplayName :: !Text
  , mdVersion :: !Text
  , mdEmail :: !Text
  , mdSupportEmail :: !Text
  , mdTermsOfUseUrl :: !Text
  , mdPrivacyUrl :: !Text
  , mdInstitution :: !Text
  , mdInstitutionUrl :: !Text
  , mdInstitutionImageUrl :: !Text
  , mdMimeType :: !Text
  , mdSupportsLanguageSyntax :: !Bool
  , mdTitle :: !Text
  , mdDescription :: !Text
  , mdQuestion :: !Text
  , mdUrl :: !Text
  , mdVideoUrl :: !Text
  , mdDisableErrorTable :: !Bool
  , mdSamples :: ![Sample]
  , mdTutorials :: ![Tutorial]
  } deriving (Show, Eq, Ord)

instance ToJSON Metadata where
  toJSON (Metadata {..}) =
    object
      [ "Name" .= mdName
      , "DisplayName" .= mdDisplayName
      , "Version" .= mdVersion
      , "Email" .= mdEmail
      , "SupportEmail" .= mdSupportEmail
      , "TermsOfUseUrl" .= mdTermsOfUseUrl
      , "PrivacyUrl" .= mdPrivacyUrl
      , "Institution" .= mdInstitution
      , "InstitutionUrl" .= mdInstitutionUrl
      , "InstitutionImageUrl" .= mdInstitutionImageUrl
      , "MimeType" .= mdMimeType
      , "SupportsLanguageSyntax" .= mdSupportsLanguageSyntax
      , "Title" .= mdTitle
      , "Description" .= mdDescription
      , "Question" .= mdQuestion
      , "Url" .= mdUrl
      -- , "VideoUrl" .= mdVideoUrl
      , "DisableErrorTable" .= mdDisableErrorTable
      , "Samples" .= mdSamples
      -- , "Tutorials" .= mdTutorials
      ]

llreveVersion :: Text
llreveVersion = "1.0"

debugExample :: Text
debugExample = debugExample0 <> "-----" <> debugExample1
  where
    debugExample0 =
      Text.unlines
        [ "extern int __mark(int);"
        , "int f(int n) {"
        , "  int i = 0;"
        , "  int j = 0;"
        , ""
        , "  while (__mark(42) & (i <= n)) {"
        , "    i++;"
        , "    j++;"
        , "  }"
        , "  return j;"
        , "}"
        ]
    debugExample1 =
      Text.unlines
        [ "extern int __mark(int);"
        , "int f(int n) {"
        , "  int i = n;"
        , "  int j = 0;"
        , ""
        , "  while (__mark(42) & (i >= 0)) {"
        , "    i = i - 1;"
        , "    j++;"
        , "  }"
        , "  return j;"
        , "}"
        ]

llreveMetadata :: Metadata
llreveMetadata =
  Metadata
  { mdName = "llreve"
  , mdDisplayName = "llrêve"
  , mdVersion = llreveVersion
  , mdEmail = "moritz.kiefer@purelyfunctional.org"
  , mdSupportEmail = "moritz.kiefer@purelyfunctional.org"
  , mdTermsOfUseUrl = "http://formal.iti.kit.edu/projects/improve/reve/"
  , mdPrivacyUrl = "http://formal.iti.kit.edu/projects/improve/reve/"
  , mdInstitution = "Karlsruhe Institute of Technology"
  , mdInstitutionUrl = "https://formal.iti.kit.edu/index.phtml"
  , mdInstitutionImageUrl = "https://formal.iti.kit.edu/img/intern/kit_logo.png"
  , mdMimeType = "text/plain"
  , mdSupportsLanguageSyntax = False
  , mdTitle = "Something something equivalence"
  , mdDescription = "Something something equivalence description"
  , mdQuestion = "Are these programs equivalent?"
  , mdUrl = "http://formal.iti.kit.edu/projects/improve/reve/"
  , mdVideoUrl = ""
  , mdDisableErrorTable = False
  , mdSamples = [Sample "loop" debugExample]
  , mdTutorials = []
  }

data RunRequest = RunRequest
  { runReqVersion :: !Text
  , runReqSource :: !Text
  } deriving (Show, Eq, Ord)

instance FromJSON RunRequest where
  parseJSON (Object v) = do
    version <- v .: "Version"
    source <- v .: "Source"
    pure (RunRequest version source)
  parseJSON _ = empty

type Rise4funAPI = "rise4fun" :> (("metadata" :> Get '[JSON] Metadata) :<|>
                                  ("run"  :> ReqBody '[JSON] RunRequest :> Post '[JSON] RunResponse))

data RunResponse = RunResponse
  { runRespVersion :: !Text
  , runRespOutputs :: ![ToolOutput]
  } deriving (Show, Eq, Ord)

instance ToJSON RunResponse where
  toJSON (RunResponse version outputs) =
    object ["Version" .= version, "Outputs" .= outputs]

data ToolOutput = ToolOutput
  { toolMimeType :: !Text
  , toolValue :: !Text
  } deriving (Show, Eq, Ord)

instance ToJSON ToolOutput where
  toJSON (ToolOutput mimetype value) =
    object ["MimeType" .= mimetype, "Value" .= value]

-- | A line is a splitter line if it starts with 5 '-'
isSplitterLine :: Text -> Bool
isSplitterLine text = Text.replicate 5 "-" `Text.isPrefixOf` text

rise4funServer :: Maybe String -> Sem -> Sem -> Server Rise4funAPI
rise4funServer includeDir queuedReqs concurrentReqs =
  return llreveMetadata :<|>
  handleRun includeDir queuedReqs concurrentReqs

responseToRunResponse :: Response -> RunResponse
responseToRunResponse (Response result _llreveOutput _solverOutput _smt _invariants _method) =
  RunResponse llreveVersion [ToolOutput "text/plain" (Text.pack (show result))]

-- TODO we should limit the number of concurrent requests here
handleRun :: Maybe String -> Sem -> Sem -> RunRequest -> ExceptT ServantErr IO RunResponse
handleRun includeDir queuedReqs concurrentReqs (RunRequest _ source) =
  withQueuedSem queuedReqs $
  withSystemTempFile "prog1.c" $ \file1 prog1Handle ->
    withSystemTempFile "prog2.c" $ \file2 prog2Handle ->
      withSystemTempFile "query.smt2" $ \smtFile smtHandle -> do
        liftIO $ do
          Text.hPutStr prog1Handle prog1
          Text.hPutStr prog2Handle prog2
          hClose prog1Handle
          hClose prog2Handle
          hClose smtHandle
        withSem concurrentReqs $ do
          resp <-
            runLlreve
              file1
              file2
              smtFile
              (llreveArgsForSolver Z3)
              includeDir
              (SolverResponse Z3)
          responseToRunResponse <$>
            case resp of
              Left resp' -> pure resp'
              Right llreveOut ->
                runSolver file1 file2 smtFile llreveOut (solverConfig Z3)
  where
    (prog1', prog2') =
      break isSplitterLine (Text.lines source)
    prog2 = Text.unlines (drop 1 prog2')
    prog1 = Text.unlines prog1'
