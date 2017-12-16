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

import           Data.Semigroup      ((<>))
import           Control.Applicative ((<**>), optional)
import           Options.Applicative (execParser, option, strOption, infoOption,
                                      switch, argument, long, short, metavar,
                                      help, hidden, auto, maybeReader,
                                      hsubparser, command, info, progDesc,
                                      fullDesc, header, helper)

import           Data.Conduit ((.|), runConduitRes, yield)

import           Text.Read (readMaybe)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Numeric.Natural (Natural)

import           Control.Exception  (evaluate)
import           Control.Concurrent (threadDelay)
import           System.Timeout     (timeout)

import           Data.Version      (showVersion)
import           Paths_passman_cli (version)
import qualified Passman.Core.Version as Passman.Core

import           Passman.Core.Config (Config (Config),
                                      masterPasswordHash, passlistPath)
import           Passman.Core.Mode   (Mode, readMode)
import           Passman.Core.Info   (Info (Info))
import           Passman.Core.Entry  (Entry (Entry))
import           Passman.Core.Hash   (generatePassword)
import qualified Passman.Core.Entry  as Entry

import           Passman.CLI.MasterPassword
import           Passman.CLI.Interaction
import           Passman.CLI.Entry
import           Passman.CLI.Config
import qualified Passman.CLI.Clipboard as Clip


data Command = ListEntries
             | AddEntry (Maybe Info) (Maybe Mode) (Maybe Natural)
             | Generate Int
             | Reconfigure Bool Bool
  deriving Show

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (parser <**> helper <**> versioner)
               ( fullDesc
              <> header "passman-cli - deterministic password generator" )

    parser = hsubparser $ mconcat
               [ command "list" $ info listParser $
                     progDesc "List entries from the passlist"
               , command "add" $ info addParser $
                     progDesc "Add an entry to the passlist"
               , command "generate" $ info generateParser $
                     progDesc "Generate password"
               , command "reconfigure" $ info reconfigureParser $
                     progDesc "Change some or all fields of configuration"
               ]

    listParser = pure ListEntries

    addParser = AddEntry
             <$> optional (strOption
                   ( long "info"
                  <> short 'i'
                  <> metavar "INFO"
                  <> help "Info string for new entry" ))
             <*> optional (option (maybeReader readMode)
                   ( long "mode"
                  <> short 'M'
                  <> metavar "MODE"
                  <> help "Mode for new entry" ))
             <*> optional (option auto
                   ( long "max-length"
                  <> short 'x'
                  <> metavar "LENGTH"
                  <> help "Maximum length for new entry (0 for no maximum)" ))

    generateParser = Generate
                  <$> argument auto
                        ( metavar "ENTRY"
                       <> help "Entry to generate password for"
                        )

    reconfigureParser = Reconfigure
                     <$> switch
                           ( long "master-password"
                          <> help "Reconfigure master password" )
                     <*> switch
                           ( long "passlist-path"
                          <> help "Reconfigure passlist path" )

    versioner = infoOption versionString
                  ( long "version"
                 <> short 'V'
                 <> help "Print version information"
                 <> hidden )

    versionString = "passman-cli version " <> showVersion version
               <> "\npassman-core version " <> showVersion Passman.Core.version
               <> "\n"
               <> "\nCopyright 2017 Matthew Harm Bekkema"
               <> "\nLicense GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>"

run :: Command -> IO ()
run ListEntries =
    T.putStr . renderList =<< loadList . passlistPath =<< getConfig

run (AddEntry i' m' l') = do
    config <- getConfig
    putStrLn "New entry"
    putStrLn "========="
    i <- maybe (Info . T.pack <$> askString "info: ") pure i'
    m <- maybe (askString "mode: " `withValidation` readMode) pure m'
    l <- maybe (askString "maximum length (0 for no maximum): " `withValidation`
                readMaybe) pure l'
    runConduitRes $ yield (Entry i l m) .| Entry.append (passlistPath config)

run (Generate i) = do
    config <- getConfig
    entry <- loadEntry i $ passlistPath config
    T.putStrLn $ renderEntry entry
    mp <- askMasterPassword $ masterPasswordHash config
    p <- evaluate $ generatePassword entry mp
    Clip.withInitialSetup $ \s -> do
        putStrLn "Password now in clipboard..."
        _ <- timeout 5000000 $ Clip.setClipboardString s p >>
                               threadDelay 5000000
        putStrLn "Clearing clipboard..."
        Clip.clearClipboard s

run (Reconfigure False False) = run $ Reconfigure True True
run (Reconfigure mph'  plp') = do
    mConfig <- tryLoadConfig
    case mConfig of
        Left e -> do
            putStrLn e
            configure Nothing Nothing
        Right (Config mph plp) -> configure (Just (mph', mph))
                                            (Just (plp', plp))
