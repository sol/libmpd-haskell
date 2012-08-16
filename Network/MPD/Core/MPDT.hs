{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables #-}

module Network.MPD.Core.MPDT (
      MPD
    , Host
    , Port
    , Password
    , Response
    , withMPDEx
    , open
    , close
    , send
    , getVersion
    , getPassword
    , setPassword
    ) where

import           Network.MPD.Util
import           Network.MPD.Core.Error

import           Data.Char (isDigit)
import           Control.Applicative (Applicative(..), (<$>), (<*))
import qualified Control.Exception as E
import           Control.Monad (ap, unless)
import           Control.Monad.Error (ErrorT(..), MonadError(..))
import           Control.Monad.Reader (ReaderT(..), ask)
import           Control.Monad.State (StateT, MonadIO(..), modify, gets, evalStateT)
import qualified Data.Foldable as F
import           Network (PortID(..), withSocketsDo, connectTo)
import           System.IO (Handle, hPutStrLn, hReady, hClose, hFlush)
import           System.IO.Error (isEOFError)
import qualified System.IO.UTF8 as U
import           Text.Printf (printf)

import qualified Prelude
import           Prelude hiding (break, drop, dropWhile, read)
import           Data.ByteString.Char8 (ByteString, isPrefixOf, dropWhile)
import qualified Data.ByteString.Char8 as B

--
-- Data types.
--

type Password = String

type Host = String
type Port = Integer

--
-- IO based MPD client implementation.
--

-- | The main implementation of an MPD client.  It actually connects
--   to a server and interacts with it.
--
-- To use the error throwing\/catching capabilities:
--
-- > import Control.Monad.Error (throwError, catchError)
--
-- To run IO actions within the MPD monad:
--
-- > import Control.Monad.Trans (liftIO)

newtype MPD a =
    MPD { runMPD :: ErrorT MPDError
                    (StateT MPDState
                     (ReaderT (Host, Port) IO)) a
        } deriving (Functor, Monad, MonadIO, MonadError MPDError)

instance Applicative MPD where
    (<*>) = ap
    pure  = return

-- | Inner state for MPD
data MPDState =
    MPDState { stHandle   :: Maybe Handle
             , stPassword :: String
             , stVersion  :: (Int, Int, Int)
             }

-- | A response is either an 'MPDError' or some result.
type Response = Either MPDError

-- | The most configurable API for running an MPD action.
withMPDEx :: Host -> Port -> Password -> MPD a -> IO (Response a)
withMPDEx host port pw x = withSocketsDo $
    runReaderT (evalStateT (runErrorT . runMPD $ open >> (x <* close)) initState)
               (host, port)
    where initState = MPDState Nothing pw (0, 0, 0)

getPassword :: MPD String
getPassword = MPD $ gets stPassword

setPassword :: String -> MPD ()
setPassword pw = MPD $ modify (\st -> st { stPassword = pw })

getVersion :: MPD (Int, Int, Int)
getVersion = MPD $ gets stVersion

open :: MPD ()
open = MPD $ do
    (host, port) <- ask
    runMPD close
    mHandle <- liftIO (safeConnectTo host port)
    modify (\st -> st { stHandle = mHandle })
    F.forM_ mHandle $ \_ -> runMPD checkConn >>= (`unless` runMPD close)
    where
        safeConnectTo host@('/':_) _ =
            (Just <$> connectTo "" (UnixSocket host))
            `E.catch` (\(_ :: E.SomeException) -> return Nothing)
        safeConnectTo host port =
            (Just <$> connectTo host (PortNumber $ fromInteger port))
            `E.catch` (\(_ :: E.SomeException) -> return Nothing)
        checkConn = do
            [msg] <- send ""
            if "OK MPD" `isPrefixOf` msg
                then MPD $ checkVersion $ parseVersion msg
                else return False

        checkVersion Nothing = throwError $ Custom "Couldn't determine MPD version"
        checkVersion (Just version)
            | version < requiredVersion =
                throwError $ Custom $ printf
                    "MPD %s is not supported, upgrade to MPD %s or above!"
                    (formatVersion version) (formatVersion requiredVersion)
            | otherwise = do
                modify (\st -> st { stVersion = version })
                return True
            where
                requiredVersion = (0, 15, 0)

        parseVersion = parseTriple '.' parseNum . dropWhile (not . isDigit)

        formatVersion :: (Int, Int, Int) -> String
        formatVersion (x, y, z) = printf "%d.%d.%d" x y z


close :: MPD ()
close =
    MPD $ do
        mHandle <- gets stHandle
        F.forM_ mHandle $ \h -> do
          modify $ \st -> st{stHandle = Nothing}
          r <- liftIO $ sendClose h
          F.forM_ r throwError
    where
        sendClose handle =
            (hPutStrLn handle "close" >> hReady handle >> hClose handle >> return Nothing)
            `E.catch` handler

        handler err
            | isEOFError err = return Nothing
            | otherwise      = (return . Just . ConnectionError) err

send :: String -> MPD [ByteString]
send str = send' `catchError` handler
    where
        handler err
          | ConnectionError e <- err, isEOFError e =  open >> send'
          | otherwise = throwError err

        send' :: MPD [ByteString]
        send' = getHandle >>= go

        go handle = MPD $ do
            unless (null str) $
                liftIO $ U.hPutStrLn handle str >> hFlush handle
            liftIO ((Right <$> getLines handle []) `E.catch` (return . Left))
                >>= either (\err -> modify (\st -> st { stHandle = Nothing })
                                 >> throwError (ConnectionError err))
                           return

        getLines :: Handle -> [ByteString] -> IO [ByteString]
        getLines handle acc = do
            l <- B.hGetLine handle
            if "OK" `isPrefixOf` l || "ACK" `isPrefixOf` l
                then (return . reverse) (l:acc)
                else getLines handle (l:acc)

        -- Return a handle to MPD.  Establish a connection, if necessary.
        getHandle :: MPD Handle
        getHandle = get >>= maybe tryReconnect return
            where
                get = MPD (gets stHandle)
                tryReconnect = open >> get >>= maybe (throwError NoMPD) return
