-- Copyright (C) 2017  Matthew Harm Bekkema
--
-- This file is part of passman-cli.
--
-- passman-cli is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- passman-cli is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.

module Passman.CLI.Config
    ( getConfig
    , configure
    , tryLoadConfig
    ) where

import Data.Bifunctor (first)
import Data.Yaml      (prettyPrintParseException)
import Data.List      (dropWhile, dropWhileEnd)
import Data.Text      (Text)
import Data.Char      (isSpace)

import Control.Exception (try)
import System.Exit       (exitSuccess)

import Passman.Core.Config (Config (Config), loadConfig, saveConfig)

import Passman.CLI.Interaction
import Passman.CLI.MasterPassword


getConfig :: IO Config
getConfig = do
    mConfig <- tryLoadConfig
    case mConfig of
        Right c -> pure c
        Left e -> do
            putStrLn e
            mkNew <- askBool "Failed to load config, create a new one <y/n>? "
            if mkNew then configure Nothing Nothing >> getConfig
                     else exitSuccess

-- | Create new configuration file. The `Bool` parameters determine whether the
-- defaults can be overriden by the user.
configure :: Maybe (Bool, Text)     -- ^ Default `masterPasswordHash`
          -> Maybe (Bool, FilePath) -- ^ Default `passlistPath`
          -> IO ()
configure mph' plp' = do
    putStrLn "New configuration"
    putStrLn "================="
    mph <- askNewMasterPassword mph'
    plp <- case plp' of
        Nothing -> askString "passlist path: "
        Just (True, dPlp) -> askString "passlist path (or blank for old): "
                             `withValidationE` orDefault Right
                             >>= maybe (pure dPlp) (pure . strip)
        Just (False, dPlp) -> pure dPlp
    saveConfig $ Config mph plp

tryLoadConfig :: IO (Either String Config)
tryLoadConfig = first prettyPrintParseException <$> try loadConfig

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
