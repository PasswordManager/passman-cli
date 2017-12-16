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

{-# LANGUAGE CPP #-}

module Passman.CLI.Clipboard
    ( Clip.withInitialSetup
    , Clip.setClipboardString
    , Clip.clearClipboard
    ) where


#ifdef DUMMYCLIPBOARD
import qualified Passman.CLI.Clipboard.Dummy as Clip
#else
import qualified Passman.CLI.Clipboard.X11 as Clip
#endif
