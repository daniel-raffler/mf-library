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

-- | Definition of atomic values, that is variables and literals
module MachineF.Atoms (
  Value (..),
  Id
  ) where

import Data.Text.Lazy (Text)

-- | Represention of integer/bool literals
data Value = IVal Int | BVal Bool
  deriving Show

-- | Representation of a variable name.
--   Used for both functions and operators
type Id = Text
