{-# LANGUAGE OverloadedStrings #-}

-- mf-library
--
-- Licensed under Creative Commons Legal Code
-- Daniel Raffler 2022
--
-- Abstract machine for a lazy functional language
-- This library defines the bytecode for the machine and provides
-- functions to read and write executable files that have been created
-- by the compiler
--
-- All work based on the paper "Übersetzerbau – Abstrakte Maschinen"
-- by François Bry and Norbert Eisinger

-- | Library to read and write exe files.
--   Defines operations and structures of the abstract machine
module MachineF (
  module MachineF.ByteCode,
  module MachineF.Atoms,
  version,
  operators,
  arity
  )where

import qualified Data.Map as Map

import MachineF.ByteCode
import MachineF.Atoms

import Data.Text.Lazy (Text)
import Data.Map ((!))

-- | Version(s) of the file format supported by this library
version :: Text
version = "1.0"

-- | List of built-in operators
operators :: [Text]
operators = ["*","/","+","-","~","==","<","not","&","|","if"]

-- | Return arity of a built-in operator
arity :: Text -> Int
arity op = table!op
  where table = Map.fromList $
          zip operators [2,2,2,2,1,2,2,1,2,2,3]
