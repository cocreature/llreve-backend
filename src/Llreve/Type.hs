{-# LANGUAGE OverloadedStrings #-}
module Llreve.Type
  ( LlreveResult(..)
  , LlreveInput(..)
  , ProgramOutput(..)
  , SMTFile(..)
  , LogMessage(..)
  , LogMessage'
  , Response(..)
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

instance ToJSON LlreveResult where
  toJSON Equivalent = "equivalent"
  toJSON NotEquivalent = "not-equivalent"
  toJSON Unknown = "unknown"
  toJSON Error = "error"

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

data Response = Response
  { respResult :: !LlreveResult
  , llreveOutput :: !Text
  , solverOutput :: !Text
  , smt :: !Text
  , respInvariants :: ![DefineFun]
  }

instance ToJSON Response where
  toJSON (Response result llreve solver smt invariants) =
    object
      [ "result" .= result
      , "invariants" .= map ppDefineFun invariants
      , "llreve-output" .= llreve
      , "solver-output" .= solver
      , "smt" .= smt
      ]
