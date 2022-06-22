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

-- | Definitions of the abstract machine's operations and structures.
--   Provides helper functions to read and write exe files
module MachineF.ByteCode (
  Program (..),
  Info (..),
  Opcode (..),
  Addr,
  toArchive,
  writeArchive,
  toFile,
  fromArchive,
  readArchive,
  fromFile
  ) where

import qualified Data.Text.Lazy as Text
import qualified Data.List      as List
import qualified Data.Maybe     as Maybe

import Data.Text.Lazy (Text)

import qualified Codec.Archive.Zip       as Zip
import qualified Data.Text.Lazy.Encoding as Encode
import qualified Data.ByteString.Lazy    as ByteS

import Codec.Archive.Zip (Entry, Archive)

import MachineF.Atoms

-- | Representation of an address in the code
type Addr = Int

-- | Representation of an exe file.
--   Consists of a version number, a function table and the actual bytecode
data Program = Program Text [Info] [Opcode]
  deriving (Show)

-- | Representation of an entry in the function table.
--   Consists of the function name, its arity and a code address 
data Info = Info Id Int Addr
  deriving (Show)

-- | Representation of the machine's operations
data Opcode
  = Reset
  | Pushval Value
  | Pushfun Id
  | Pushpre Id
  | Pushparam Int
  | Makeapp
  | Unwind
  | Call
  | Operator Int
  | Slide Int
  | Update Int
  | Return
  | Halt
  deriving (Show)

bVersion :: Text -> Entry
bVersion version = Zip.toEntry "version" 0 (Encode.encodeUtf8 version)

bTable :: [Info] -> Entry
bTable info = Zip.toEntry "table" 0 (Encode.encodeUtf8 table)
  where table = Text.pack $ concat
          [Text.unpack fx ++ " " ++ show k ++ " " ++ show addr ++ "\n" |
            Info fx k addr <- info]

bCode :: [Opcode] -> Entry
bCode ops = Zip.toEntry "code" 0 (Encode.encodeUtf8 code)
  where bVal (IVal v) = show v
        bVal (BVal v) = show v
        
        bOp v = op v ++ "\n"
          where op  Reset        = "Reset"
                op (Pushval v)   = "Pushval " ++ bVal v
                op (Pushfun f)   = "Pushfun " ++ Text.unpack f
                op (Pushpre o)   = "Pushpre " ++ Text.unpack o
                op (Pushparam k) = "Pushparam " ++ show k
                op  Makeapp      = "Makeapp"
                op  Unwind       = "Unwind"
                op  Call         = "Call"
                op (Operator k)  = "Operator " ++ show k
                op (Slide k)     = "Slide " ++ show k
                op (Update k)    = "Update " ++ show k
                op  Return       = "Return"
                op  Halt         = "Halt"
        
        code = Text.pack $ concatMap bOp ops

-- | Pack all program data into an archive
toArchive :: Program -> Archive
toArchive (Program vx info code) = Zip.Archive [bVersion vx, bTable info, bCode code] Nothing mempty

-- | Write the archive to the disk to create an exe file
writeArchive :: FilePath -> Archive -> IO ()
writeArchive path archive = ByteS.writeFile path (Zip.fromArchive archive) 

-- | Pack program data into an archive and the write it to the disk
toFile :: FilePath -> Program -> IO ()
toFile path = writeArchive path . toArchive

pVersion :: Entry -> Text
pVersion e = if version == "1.0" then version else error "error reading exe file, library is outdated"
  where version = Encode.decodeUtf8 $ Zip.fromEntry e

pTable :: Entry -> [Info]
pTable e = [Info (Text.pack f) (read k) (read a) | [f,k,a] <- map List.words table]
  where table = List.lines $ Text.unpack $ Encode.decodeUtf8 $ Zip.fromEntry e

pCode :: Entry -> [Opcode]
pCode e = [pOp f0 fx | (f0:fx) <- map List.words code]
  where pVal "True"  = BVal True
        pVal "False" = BVal False
        pVal k       = IVal (read k)
        
        pOp "Reset"     []  = Reset
        pOp "Pushval"   [v] = Pushval $ pVal v
        pOp "Pushfun"   [v] = Pushfun $ Text.pack v
        pOp "Pushpre"   [v] = Pushpre $ Text.pack v
        pOp "Pushparam" [v] = Pushparam $ read v
        pOp "Makeapp"   []  = Makeapp
        pOp "Unwind"    []  = Unwind
        pOp "Call"      []  = Call
        pOp "Operator"  [v] = Operator $ read v
        pOp "Slide"     [v] = Slide $ read v
        pOp "Update"    [v] = Update $ read v
        pOp "Return"    []  = Return
        pOp "Halt"      [ ] = Halt
        
        code = List.lines $ Text.unpack $ Encode.decodeUtf8 $ Zip.fromEntry e

-- | Read an exe file from the disk and create the archive
readArchive :: FilePath -> IO Archive
readArchive path = Zip.toArchive <$> ByteS.readFile path

-- | Open archive and extract program data
fromArchive :: Archive -> Program
fromArchive archive = Program (pVersion $ lookup "version") (pTable $ lookup "table") (pCode $ lookup "code")
  where lookup path = Maybe.fromJust $ Zip.findEntryByPath path archive

-- | Read exe file froom the disk and extract all program data
fromFile :: FilePath -> IO Program
fromFile path = fromArchive <$> readArchive path
