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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Passman.CLI.Table
    ( Table(..)
    , Column(..)
    , mkTable
    , renderTable
    , renderRow
    ) where

import qualified Data.Text as T
import           Data.Text (Text)

import           Data.Functor.Contravariant (Contravariant (contramap))
import           Data.Semigroup             (Semigroup)
import           Data.Foldable              (foldl')
import           Data.Coerce                (coerce)


newtype Table a = Table [Column a]
  deriving (Monoid, Semigroup)

newtype Column a = Column (Text, a -> Text)

instance Contravariant Column where
    contramap f (Column (h, c)) = Column (h, c . f)

instance Contravariant Table where
    contramap f (Table xs) = Table $ map (contramap f) xs

mkTable :: [(Text, a -> Text)] -> Table a
mkTable = coerce

renderTable :: Table a -> [a] -> Text
renderTable hcs xs = T.unlines $
    map (T.intercalate $ T.singleton '\t') paddedRows
  where
    (hs, cs) = unzip $ coerce hcs
    rows = hs : map (\x -> map ($ x) cs) xs
    lengths = foldl' (zipWith (\acc x -> acc `max` T.length x)) (repeat 0) rows
    paddedRows = map (zipWith (\l x -> T.justifyLeft l ' ' x) lengths) rows

renderRow :: Table a -> a -> Text
renderRow (Table hcs) x = T.intercalate (T.singleton '\t') row
  where
    cs = map (\(Column (_,c)) -> c) hcs
    row = map ($x) cs
