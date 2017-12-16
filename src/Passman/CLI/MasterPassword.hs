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

module Passman.CLI.MasterPassword
    ( askNewMasterPassword
    , askMasterPassword
    ) where

import           Data.Function (fix)
import qualified Data.Text     as T
import           Data.Text     (Text)

import           Passman.Core.Hash (MasterPassword, masterPassword,
                                    fromMasterPassword, checkMasterPassword,
                                    hashMasterPassword)

import           Passman.CLI.Interaction


askNewMasterPassword :: Maybe (Bool, Text) -> IO Text
askNewMasterPassword Nothing = fix $ \loop ->
    askPassword "master password: " `withValidationE`
    validateMasterPassword >>=
    withConfirmationOr loop
askNewMasterPassword (Just (True, mp')) = fix $ \loop ->
    askPassword "master password (or blank for old): " `withValidationE`
    orDefault validateMasterPassword >>=
    maybe (pure mp') (withConfirmationOr loop)
askNewMasterPassword (Just (False, mp)) = pure mp

withConfirmationOr :: IO Text -> MasterPassword -> IO Text
withConfirmationOr loop mp = do
    vMp <- T.pack <$> askPassword "confirm master password: "
    if fromMasterPassword mp == vMp
        then hashMasterPassword mp
        else putStrLn "passwords do not match" >> loop

askMasterPassword :: Text -> IO MasterPassword
askMasterPassword hash = askPassword "master password: "
    `withValidationE` validateMasterPassword
    `withValidationE` validateCorrectMasterPassword hash

validateCorrectMasterPassword :: Text
                              -> MasterPassword
                              -> Either String MasterPassword
validateCorrectMasterPassword hash p = case checkMasterPassword hash p of
    False -> Left  "Incorrect master password."
    True  -> Right p

validateMasterPassword :: String -> Either String MasterPassword
validateMasterPassword = maybe (Left "Invalid master password.") Right .
    masterPassword . T.pack
