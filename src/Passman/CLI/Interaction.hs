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

module Passman.CLI.Interaction
    ( askString
    , askPassword
    , askBool
    , withValidation
    , withValidationE
    , orDefault
    ) where

import           Data.Function (fix)
import           Data.Char     (toLower)

import           System.Exit (exitSuccess)

import           System.Console.Haskeline (InputT)
import qualified System.Console.Haskeline as HL


runInputT :: InputT IO a -> IO a
runInputT = HL.runInputTWithPrefs HL.defaultPrefs HL.defaultSettings

askString :: String -> IO String
askString p = runInputT (HL.getInputLine p) >>= orExit

askPassword :: String -> IO String
askPassword p = runInputT (HL.getPassword (Just '*') p) >>= orExit

askBool :: String -> IO Bool
askBool = flip withValidation (helper . map toLower) . askString
  where
    helper "yes" = Just True
    helper "y"   = Just True
    helper "no"  = Just False
    helper "n"   = Just False
    helper _     = Nothing

withValidation :: IO a -> (a -> Maybe b) -> IO b
withValidation x v = fix $ \r -> fmap v x >>= maybe r pure

withValidationE :: IO a -> (a -> Either String b) -> IO b
withValidationE x v = fix $ \r ->
    fmap v x >>= either (\s -> putStrLn s >> r) pure

orExit :: Maybe a -> IO a
orExit (Just x) = pure x
orExit Nothing  = exitSuccess

orDefault :: (String -> Either e a) -> String -> Either e (Maybe a)
orDefault _ [] = Right Nothing
orDefault v x  = Just <$> v x
