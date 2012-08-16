{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
-- | Module    : Network.MPD.Core
-- Copyright   : (c) Ben Sinclair 2005-2009, Joachim Fasting 2010
-- License     : MIT (see LICENSE)
-- Maintainer  : Joachim Fasting <joachim.fasting@gmail.com>
-- Stability   : alpha
--
-- The core datatypes and operations are defined here, including the
-- primary instance of the 'MonadMPD' class, 'MPD'.

module Network.MPD.Core (
    -- * Classes
    MonadMPD(..),
    -- * Data types
    MPD, MPDError(..), ACKType(..), Response, Host, Port, Password,
    -- * Running
    withMPDEx,
    -- * Interacting
    getResponse, kill,
    ) where

import           Network.MPD.Core.MPDT (MPDT, Host, Port, Password, Response)
import qualified Network.MPD.Core.MPDT as MPDT
import           Network.MPD.Util
import           Network.MPD.Core.Error

import           Network (withSocketsDo)

import           Control.Monad.Error (MonadError(..), MonadIO)

import qualified Prelude
import           Prelude hiding (break, drop, dropWhile, read)
import           Data.ByteString.Char8 (ByteString, isPrefixOf, break, drop, dropWhile)
import qualified Data.ByteString.UTF8 as UTF8

type MPD = MPDT IO

-- | The most configurable API for running an MPD action.
withMPDEx :: Host -> Port -> Password -> MPD a -> IO (Response a)
withMPDEx host port pw = withSocketsDo . MPDT.runMPDT host port pw

-- | A typeclass to allow for multiple implementations of a connection
--   to an MPD server.
class (Monad m, MonadError MPDError m) => MonadMPD m where
    -- | Open (or re-open) a connection to the MPD server.
    open  :: m ()
    -- | Close the connection.
    close :: m ()
    -- | Send a string to the server and return its response.
    send  :: String -> m [ByteString]
    -- | Produce a password to send to the server should it ask for
    --   one.
    getPassword :: m Password
    -- | Alters password to be sent to the server.
    setPassword :: Password -> m ()
    -- | Get MPD protocol version
    getVersion :: m (Int, Int, Int)

instance (MonadIO m) => MonadMPD (MPDT m) where
    open        = MPDT.open
    close       = MPDT.close
    send        = MPDT.send
    getPassword = MPDT.getPassword
    setPassword = MPDT.setPassword
    getVersion  = MPDT.getVersion

--
-- Other operations.
--

-- | Kill the server. Obviously, the connection is then invalid.
kill :: (MonadMPD m) => m ()
kill = send "kill" >> return ()

-- | Send a command to the MPD server and return the result.
getResponse :: (MonadMPD m) => String -> m [ByteString]
getResponse cmd = (send cmd >>= parseResponse) `catchError` sendpw
    where
        sendpw e@(ACK Auth _) = do
            pw <- getPassword
            if null pw then throwError e
                else send ("password " ++ pw) >>= parseResponse
                  >> send cmd >>= parseResponse
        sendpw e =
            throwError e

-- Consume response and return a Response.
parseResponse :: (MonadError MPDError m) => [ByteString] -> m [ByteString]
parseResponse xs
    | null xs                    = throwError $ NoMPD
    | "ACK" `isPrefixOf` x       = throwError $ parseAck x
    | otherwise                  = return $ Prelude.takeWhile ("OK" /=) xs
    where
        x = head xs

-- Turn MPD ACK into the corresponding 'MPDError'
parseAck :: ByteString -> MPDError
parseAck s = ACK ack (UTF8.toString msg)
    where
        ack = case code of
                2  -> InvalidArgument
                3  -> InvalidPassword
                4  -> Auth
                5  -> UnknownCommand
                50 -> FileNotFound
                51 -> PlaylistMax
                52 -> System
                53 -> PlaylistLoad
                54 -> Busy
                55 -> NotPlaying
                56 -> FileExists
                _  -> UnknownACK
        (code, _, msg) = splitAck s

-- Break an ACK into (error code, current command, message).
-- ACKs are of the form:
-- ACK [error@command_listNum] {current_command} message_text\n
splitAck :: ByteString -> (Int, ByteString, ByteString)
splitAck s = (read code, cmd, msg)
    where
        (code, notCode) = between '[' '@' s
        (cmd, notCmd)   = between '{' '}' notCode
        msg             = drop 1 $ dropWhile (' ' ==) notCmd

        -- take whatever is between 'f' and 'g'.
        between a b xs  = let (_, y) = break (== a) xs
                          in break (== b) (drop 1 y)
