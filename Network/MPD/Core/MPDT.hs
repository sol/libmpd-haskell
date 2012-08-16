{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, ScopedTypeVariables #-}

module Network.MPD.Core.MPDT (
      Host
    , Port
    , Password
    , Response
    , MPDT
    , runMPDT
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
import           Control.Applicative
import qualified Control.Exception as E
import           Control.Monad (ap, unless)
import           Control.Monad.Error (ErrorT(..), MonadError(..))
import           Control.Monad.Reader (ReaderT(..), ask)
import           Control.Monad.State (StateT, MonadIO(..), modify, gets, evalStateT)
import qualified Data.Foldable as F
import           Network (PortID(..), connectTo)
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
newtype MPDT m a =
    MPDT { unMPDT :: ErrorT MPDError
                    (StateT MPDState
                     (ReaderT (Host, Port) m)) a
        } deriving (Functor, Monad, MonadIO, MonadError MPDError)

instance (Functor m, Monad m) => Applicative (MPDT m) where
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

runMPDT :: MonadIO m => Host -> Port -> Password -> MPDT m a -> m (Response a)
runMPDT host port pw action =
    (`runReaderT` config) . (`evalStateT` initState) . runErrorT . unMPDT $ do
        open
        r <- action
        close
        return r
    where
      config = (host, port)
      initState = MPDState Nothing pw (0, 0, 0)


getPassword :: Monad m => MPDT m String
getPassword = MPDT $ gets stPassword

setPassword :: Monad m => String -> MPDT m ()
setPassword pw = MPDT $ modify (\st -> st { stPassword = pw })

getVersion :: Monad m => MPDT m (Int, Int, Int)
getVersion = MPDT $ gets stVersion

open :: MonadIO m => MPDT m ()
open = MPDT $ do
    (host, port) <- ask
    unMPDT close
    mHandle <- liftIO (safeConnectTo host port)
    modify (\st -> st { stHandle = mHandle })
    F.forM_ mHandle $ \_ -> unMPDT checkConn >>= (`unless` unMPDT close)
    where
        safeConnectTo host@('/':_) _ =
            (Just <$> connectTo "" (UnixSocket host))
            `E.catch` (\(_ :: E.SomeException) -> return Nothing)
        safeConnectTo host port =
            (Just <$> connectTo host (PortNumber $ fromInteger port))
            `E.catch` (\(_ :: E.SomeException) -> return Nothing)

        checkConn :: MonadIO m => MPDT m Bool
        checkConn = do
            [msg] <- send ""
            if "OK MPD" `isPrefixOf` msg
                then MPDT $ checkVersion $ parseVersion msg
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


close :: MonadIO m => MPDT m ()
close =
    MPDT $ do
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

send :: MonadIO m => String -> MPDT m [ByteString]
send str = send' `catchError` handler
    where
        handler err
          | ConnectionError e <- err, isEOFError e =  open >> send'
          | otherwise = throwError err

        send' :: MonadIO m => MPDT m [ByteString]
        send' = getHandle >>= go

        go handle = MPDT $ do
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
        getHandle :: MonadIO m => MPDT m Handle
        getHandle = get >>= maybe tryReconnect return
            where
                get = MPDT (gets stHandle)
                tryReconnect = open >> get >>= maybe (throwError NoMPD) return
