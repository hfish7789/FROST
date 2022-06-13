{- This file is part of the Haskell version of the FROST compiler.

   Copyright (C) 2019  XAIN Stichting, Amsterdam  info@xain.foundation

   The Haskell version of the FROST compiler is free software: you can
   redistribute it and/or modify it under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   The Haskell version of the FROST compiler is distributed in the hope that it
   will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
   Public License for more details.

   You should have received a copy of the GNU General Public License along with
   this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Module     : CmdOptions
--   Copyright  : (c) XAIN Stichting, 2019
--   License    : GPL-3
--   Maintainer : info@xain.foundation
--   Stability  : experimental
--
-- FROST compiler CLI options
--
module CmdOptions
  ( Opts(..), Input(..),
    opts, input
  ) where

import Data.Semigroup ((<>))
import Options.Applicative -- optparse-applicative package
  ( Parser,
    (<|>),
    strOption, switch,
    long, short, metavar, help, value, showDefault )


-- | Command line options record data type
data Opts = Opts
  { source   :: Input
  , pretty   :: Bool
  , fileless :: Bool
  , outfile  :: FilePath }

-- | Input option data type
data Input = FileIn FilePath
           | ExprIn String

-- | Parser of input option
input :: Parser Input
input = FileIn <$> strOption
      (  long "ifile"
      <> short 'i'
      <> metavar "INFILE"
      <> help "Input file" )
  <|> ExprIn <$> strOption
      (  long "iexpr"
      <> short 'x'
      <> metavar "EXPRESSION"
      <> help "FROST expression" )

-- | Parser of command line options
opts :: Parser Opts
opts = Opts
  <$> input
  <*> switch
      (  long "pretty"
      <> short 'p'
      <> help "Whether to pretty-print output" )
  <*> switch
      (  long "oconsole"
      <> short 'c'
      <> help "Whether to only output to console" )
  <*> strOption
      (  long "ofile"
      <> short 'o'
      <> metavar "OUTFILE"
      <> showDefault
      <> value "out.json"
      <> help "Output file" )
