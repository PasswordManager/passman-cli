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

{-# LANGUAGE OverloadedStrings #-}

module Passman.CLI.Entry
    ( loadList
    , loadEntry
    , renderList
    , renderEntry
    ) where

import           Data.Functor.Contravariant (contramap)
import           Data.Semigroup             ((<>))

import           Control.Monad.Trans.Resource (runResourceT)

import           Data.Conduit ((.|), runConduitRes, sourceToList, await)
import qualified Data.Conduit.List as C

import           Data.Text (Text)
import qualified Data.Text as T

import           Passman.Core.Entry (Entry, info, maxLength, mode, load)
import           Passman.Core.Mode  (characterCode)
import           Passman.Core.Info  (fromInfo)

import           Passman.CLI.Table


loadList :: FilePath -> IO [Entry]
loadList = runResourceT . sourceToList . load

loadEntry :: Int -> FilePath -> IO Entry
loadEntry i fp = if i < 0
    then fail "Invalid negative index"
    else runConduitRes $ load fp
                      .| (C.drop i >>
                          await >>=
                          maybe (fail "Index out of bounds") pure)

renderList :: [Entry] -> Text
renderList = renderTable indexedPassList . zip [0..]

renderEntry :: Entry -> Text
renderEntry = renderRow passList

indexedPassList :: Table (Int, Entry)
indexedPassList = mkTable [("#", T.pack . show . fst)] <> contramap snd passList

passList :: Table Entry
passList = mkTable
    [ ("Info"  , fromInfo . info)
    , ("Length", T.pack . (\l -> if l == 0 then "Max" else show l) . maxLength)
    , ("Mode"  , characterCode . mode)
    ]
