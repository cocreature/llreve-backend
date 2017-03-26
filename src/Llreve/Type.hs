{-# LANGUAGE OverloadedStrings #-}
module Llreve.Type
  ( LlreveResult(..)
  , LlreveInput(..)
  , ProgramOutput(..)
  , SMTFile(..)
  , LogMessage(..)
  , LogMessage'
  , Response(..)
  , ResponseMethod(..)
  , SMTSolver(..)
  , LlreveOutput(..)
  , SolverOutput(..)
  , llreveError
  ) where

import Control.Monad.Log (WithSeverity)
import Data.Aeson hiding (Error)
import Data.Text (Text)
import Horname

data LlreveResult
  = Equivalent
  | NotEquivalent
  | Unknown
  | Error
  | Timeout
  deriving (Show, Eq, Ord)

instance ToJSON LlreveResult where
  toJSON Equivalent = "equivalent"
  toJSON NotEquivalent = "not-equivalent"
  toJSON Unknown = "unknown"
  toJSON Error = "error"
  toJSON Timeout = "timeout"

data LlreveInput = LlreveInput
  { inpProgram1 :: !Text
  , inpProgram2 :: !Text
  } deriving (Show, Eq, Ord)

data ProgramOutput = ProgramOutput
  { progOut :: !Text
  } deriving (Show, Eq, Ord)

newtype SMTFile =
  SMTFile Text
  deriving (Show, Eq, Ord)

type LogMessage' = WithSeverity LogMessage

data LogMessage
  = LlreveMsg { llreveMsg :: !Text
             ,  llreveOutp :: !ProgramOutput
             ,  llreveInp :: !LlreveInput}
  | Z3Msg { z3Msg :: !Text
         ,  z3Inp :: !SMTFile
         ,  z3Outp :: !ProgramOutput
         ,  llreveInp :: !LlreveInput}
  | HornameMsg { hornameMsg :: !Text
              ,  hornameSolverInp :: !Text
              ,  hornameSolverOutp :: !Text}
  | EldaricaMsg { eldaricaMsg :: !Text
               ,  eldInput :: !SMTFile
               ,  eldOutp :: !ProgramOutput
               ,  llreveInp :: !LlreveInput}
  deriving (Show, Eq, Ord)

data SMTSolver
  = Eldarica
  | Z3
  deriving (Show, Eq, Ord)

instance ToJSON SMTSolver where
  toJSON Eldarica = "eldarica"
  toJSON Z3 = "z3"

data ResponseMethod = SolverResponse !SMTSolver | DynamicResponse

instance ToJSON ResponseMethod where
  toJSON (SolverResponse solver) = toJSON solver
  toJSON DynamicResponse = "dynamic"

data LlreveOutput = LlreveOutput
  { llreveStdout :: !Text
  , llvmIr :: !(Text, Text)
  , llreveSmt :: !Text
  }

data SolverOutput = SolverOutput
  { solverStdout :: !Text
  , solverInvariants :: ![DefineFun]
  , solverResult :: !LlreveResult
  }

data Response = Response
  { llreveOutput :: !LlreveOutput
  , solverOutput :: !SolverOutput
  , respMethod :: !ResponseMethod
  }

llreveError :: LlreveOutput -> ResponseMethod -> Response
llreveError output method = Response output (SolverOutput "" [] Error) method

instance ToJSON Response where
  toJSON (Response (LlreveOutput llreve (ir1, ir2) smt') (SolverOutput solverOut invariants result) method) =
    object
      [ "result" .= result
      , "invariants" .= map ppDefineFun invariants
      , "llreve-output" .= llreve
      , "llvm-ir-1" .= ir1
      , "llvm-ir-2" .= ir2
      , "solver-output" .= solverOut
      , "smt" .= smt'
      , "method" .= method
      ]
