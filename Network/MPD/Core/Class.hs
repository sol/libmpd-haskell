{-# LANGUAGE FlexibleContexts #-}

-- | Module    : Network.MPD.Core.Class
-- Copyright   : (c) Ben Sinclair 2005-2009
-- License     : LGPL (see LICENSE)
-- Maintainer  : bsinclai@turing.une.edu.au
-- Stability   : alpha
-- Portability : unportable (uses FlexibleContexts)
--
-- The MPD typeclass.

module Network.MPD.Core.Class where

import Network.MPD.Core.Error (MPDError)

import Control.Monad.Error (MonadError)

type Password = String

-- | A typeclass to allow for multiple implementations of a connection
--   to an MPD server.
class (Monad m, MonadError MPDError m) => MonadMPD m where
    -- | Open (or re-open) a connection to the MPD server.
    open  :: m ()
    -- | Close the connection.
    close :: m ()
    -- | Send a string to the server and return its response.
    send  :: String -> m String
    -- | Produce a password to send to the server should it ask for
    --   one.
    getPassword :: m Password