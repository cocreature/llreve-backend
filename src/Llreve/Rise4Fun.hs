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
  , Rise4funRequest(..)
  , Rise4funResponse(..)
  , ToolOutput(..)
  ) where

import           Control.Applicative
import           Control.Concurrent.Sem
import           Control.Monad.Except
import           Data.Aeson hiding (Error)
import           Data.List (intersperse)
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

-- This needs to be incremented to invalidate the cache of rise4fun
llreveVersion :: Text
llreveVersion = "1.0"

debugExample :: Text
debugExample = Text.unlines (debugExample0 <> ["-----"] <> debugExample1)
  where
    debugExample0 =
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
  , mdTitle = "Prove the equivalence of two C programs"
  , mdDescription =
      "llrêve can prove the equivalence of two C programs. The programs have to be separated by a line starting with '----'. Corresponding loops should be marked with __mark(id) where the id has to be the same for both programs."
  , mdQuestion = "Are these programs equivalent?"
  , mdUrl = "http://formal.iti.kit.edu/projects/improve/reve/"
  , mdVideoUrl = ""
  , mdDisableErrorTable = True
  , mdSamples = [Sample "loop" debugExample]
  , mdTutorials = []
  }

data Rise4funRequest = R4fRequest
  { runReqVersion :: !Text
  , runReqSource :: !Text
  } deriving (Show, Eq, Ord)

instance FromJSON Rise4funRequest where
  parseJSON (Object v) = do
    version <- v .: "Version"
    source <- v .: "Source"
    pure (R4fRequest version source)
  parseJSON _ = empty

type Rise4funAPI = "rise4fun" :> (("metadata" :> Get '[JSON] Metadata) :<|>
                                  ("run"  :> ReqBody '[JSON] Rise4funRequest :> Post '[JSON] Rise4funResponse))

data Rise4funResponse = R4fResponse
  { runRespVersion :: !Text
  , runRespOutputs :: ![ToolOutput]
  } deriving (Show, Eq, Ord)

instance ToJSON Rise4funResponse where
  toJSON (R4fResponse version outputs) =
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

-- for some reason proper code blocks don’t seem to work so for now we
-- just add additional lines since markdown ignores linekbreaks
-- otherwise
wrapInCodeBlock :: Text -> [Text]
wrapInCodeBlock t = intersperse "" (Text.lines t)

responseToR4fResponse :: Response -> Rise4funResponse
responseToR4fResponse (Response result llreveOutput solverOutput _smt _invariants _method) =
  case result of
    Error ->
      R4fResponse
        llreveVersion
        [ ToolOutput
            "text/x-web-markdown"
            (Text.unlines $
             ["## An error occured", "### Output of llrêve"] <>
             wrapInCodeBlock (llreveStdout llreveOutput) <>
             ["### Output of the SMT solver"] <>
             wrapInCodeBlock solverOutput)
        ]
    Equivalent ->
      R4fResponse
        llreveVersion
        [ToolOutput "text/plain" "The programs have been proven equivalent."]
    NotEquivalent ->
      R4fResponse
        llreveVersion
        [ ToolOutput
            "text/plain"
            "A difference has been detected, the programs are not equivalent."
        ]
    Unknown ->
      R4fResponse
        llreveVersion
        [ ToolOutput
            "text/plain"
            "The programs could neither be proven or disproven equivalent."
        ]
    Timeout ->
      R4fResponse
        llreveVersion
        [ ToolOutput
            "text/plain"
            "The programs could not be proven equivalent in the given timeframe."
        ]

handleRun :: Maybe String -> Sem -> Sem -> Rise4funRequest -> ExceptT ServantErr IO Rise4funResponse
handleRun includeDir queuedReqs concurrentReqs (R4fRequest _ source) =
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
          responseToR4fResponse <$>
            case resp of
              Left resp' -> pure resp'
              Right llreveOut ->
                runSolver file1 file2 smtFile llreveOut (solverConfig Z3)
  where
    (prog1', prog2') = break isSplitterLine (Text.lines source)
    prog2 = Text.unlines (drop 1 prog2')
    prog1 = Text.unlines prog1'
