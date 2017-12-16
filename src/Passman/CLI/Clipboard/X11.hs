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

{-# LANGUAGE RecordWildCards #-}

module Passman.CLI.Clipboard.X11
    ( withInitialSetup
    , setClipboardString
    , clearClipboard
    ) where

import           Control.Monad (when)
import           Data.Function (fix)
import           Data.Maybe    (isNothing)

import           Data.Text         (Text)
import qualified Data.Text.Foreign as T

import           Control.Exception  (bracket)
import           Control.Concurrent (threadDelay)
import           System.Timeout     (timeout)
import           Foreign.Ptr        (castPtr)

import Graphics.X11.Xlib        (Display, XEventPtr, Window, Time, Atom,
                                 destroyWindow, createSimpleWindow, sendEvent,
                                 pending, nextEvent, allocaXEvent, openDisplay,
                                 defaultRootWindow, closeDisplay, internAtom,
                                 getAtomName, selectionNotify)
import Graphics.X11.Xlib.Extras (Event (SelectionClear, SelectionRequest,
                                        ev_requestor, ev_selection, ev_target,
                                        ev_property, ev_time),
                                 xSetSelectionOwner, xChangeProperty,
                                 setSelectionNotify, setEventType,
                                 propModeReplace, none, getEvent, currentTime)


setClipboardString :: (Display, Window, Atom) -> Text -> IO ()
setClipboardString (display, window, clipboard) str = do
    xSetSelectionOwner display clipboard window currentTime
    advertiseSelection display str True

clearClipboard :: (Display, Window, Atom) -> IO ()
clearClipboard (display, window, clipboard) = do
    xSetSelectionOwner display clipboard window currentTime
    _ <- timeout 1000000 $
        advertiseSelection display mempty False
    return ()

interruptableNextEvent :: Display -> XEventPtr -> IO ()
interruptableNextEvent display evPtr = fix $ \r -> do
    p <- pending display
    if p > 0
        then nextEvent display evPtr
        else threadDelay 1000 >> r

advertiseSelection :: Display -> Text -> Bool -> IO ()
advertiseSelection display str loop = allocaXEvent go
  where
    go evPtr = do
        interruptableNextEvent display evPtr
        ev <- getEvent evPtr
        case ev of
            SelectionRequest {..} -> do
                target' <- getAtomName display ev_target
                res <- handleOutput display ev_requestor ev_property target' str
                sendSelectionNotify display ev_requestor ev_selection ev_target res ev_time
                when (res == none || loop) $ go evPtr

            SelectionClear {..} -> return ()

            _ -> go evPtr

handleOutput :: Display -> Window -> Atom -> Maybe String -> Text -> IO Atom
handleOutput display req prop (Just "UTF8_STRING") str = do
    prop' <- getAtomName display prop
    if isNothing prop' then handleOutput display req prop Nothing str else do
        target <- internAtom display "UTF8_STRING" True
        _ <- T.withCStringLen str $ \(str', len) ->
            xChangeProperty display req prop target 8 propModeReplace
                            (castPtr str') (fromIntegral len)
        return prop
handleOutput _ _ _ _ _ = return none

sendSelectionNotify :: Display -> Window -> Atom -> Atom -> Atom -> Time -> IO ()
sendSelectionNotify display req sel target prop time = allocaXEvent $ \ev -> do
    setEventType ev selectionNotify
    setSelectionNotify ev req sel target prop time
    sendEvent display req False 0 ev

withInitialSetup :: ((Display, Window, Atom) -> IO a) -> IO a
withInitialSetup f = bracket (openDisplay "") closeDisplay $ \display ->
    bracket (createSimpleWindow display (defaultRootWindow display)
                                0 0 1 1 0 0 0)
            (destroyWindow display) $ \window -> do
        clipboard <- internAtom display "CLIPBOARD" False
        f (display, window, clipboard)
