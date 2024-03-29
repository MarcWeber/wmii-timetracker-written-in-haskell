{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import System.Posix.IO (LockRequest(..))
import System.Posix.IO (waitToSetLock)
import System.Posix.IO (handleToFd)
import Data.Char (isSpace)
import Control.Exception
import Control.Concurrent
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Data.Function (on)
import Text.Printf

import System.Time
import System.Locale

import WMII
import Data.List hiding ( insert, delete )
import Data.IORef
import Data.Map hiding ( insert, (!), map, delete )
import qualified Data.Map as M
import GHC.Read
import Control.Monad
import System.IO.Unsafe
import qualified Control.Monad.Reader
import Data.Time
import Data.Maybe
import System.IO
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as BS

import Text.ParserCombinators.Parsec.Char as PC
import Text.ParserCombinators.Parsec as PC
import Text.Parsec.String

times :: MVar (M.Map String Int) -- name, seconds
times = unsafePerformIO $ newMVar $ M.empty

atTag :: MVar (String, start)
atTag = unsafePerformIO $ newEmptyMVar

logErr = hPutStrLn stderr

-- tracking wmii events {{{

addTimeCode :: WmiiEvent -> IO ( WmiiEvent, CalendarTime )
addTimeCode event = fmap ( (,) event) (getClockTime >>= toCalendarTime)

start tag time = putMVar atTag (tag, time)
stop tag time = do
  mbStart <- tryTakeMVar atTag
  case mbStart of
    Just (startTag, startTime) -> do
      if startTag /= tag then logErr "unexpected tag missmatch !?"
        else do
          let diff_sec = tdSec ( (diffClockTimes `on` toClockTime) time startTime )
          modifyMVar_ times $ return . M.insertWith (+) tag diff_sec
          print (tag, diff_sec)
          -- print =<< readMVar times

    Nothing -> logErr $ "orphan stop event " ++ show (tag, time)

cleanTag s = if "max_" `isPrefixOf` s then drop 4 s else s

handleEvent :: (WmiiEvent, CalendarTime) -> IO ()
handleEvent (FocusTag tag, time) = start (cleanTag tag) time
handleEvent (UnfocusTag tag, time) = stop (cleanTag tag) time
handleEvent _ = return ()

trackActions = do events <- wmiiActions
                  mapM_ (\act -> addTimeCode act >>= handleEvent) events
               
usage fn = unlines [ "usage of " ++ fn
		    , "" ]

printValueOf :: (Maybe String) -> IO ()
printValueOf mbTag = do
        case mbTag of
          Just n -> print =<< (liftM (M.lookup n) $ readMVar times)
          Nothing -> mapM_ print =<< (liftM toList $ readMVar times)

-- commandline {{{
parseLine :: String -> (Either ParseError (IO ()))
parseLine s = runParser p () "line" s 
  where
      p :: CharParser () (IO ())
      p = (choice (map PC.try [ showCmd, resetCmd ]))
      tag = many1 $ satisfy $ not . isSpace

      showCmd = do
        string "show"
        mbTag <- optionMaybe $ PC.try $ (many1 $ satisfy isSpace) >> tag
        eof
        return $ printValueOf mbTag
      
      resetCmd = do
        string "reset"
        many1 $ satisfy isSpace
        t <- tag
        return $ do
          printValueOf (Just t)
          modifyMVar_ times $ return . M.insert t 0

readCommandsFromStdin = do
  l <- getLine
  either print id $ parseLine l
  readCommandsFromStdin

-- save {{{
save storage = do
  print $ "writing to " ++ storage
  writeFile storage =<< fmap show (readMVar times)

savePeriodically x = do
  threadDelay $ 60
              * 1000000 -- sec
  save x
  savePeriodically x

-- main {{{
forever'     :: (Monad m) => m a -> m b
forever' a   = a >> forever' a

main = do
  storage <- liftM (</> ".timetracker-data") getHomeDirectory
  lock <- liftM (</> ".timetracker-data.lock") getHomeDirectory

  -- aquire lock
  putStrLn "aquiring lock"
  lockfd<- handleToFd -- closes handle
            =<< openFile lock WriteMode
  waitToSetLock lockfd (WriteLock, AbsoluteSeek, 0, 0)

  handle (\(e :: SomeException) -> do
      save storage
    ) $ do
    de <- doesFileExist  storage
    when de $ do
      print $ "reading from: " ++ storage
      modifyMVar_ times $ \_ -> liftM (read . BS.unpack) $ BS.readFile storage -- only works with ASCII names!
      print =<< readMVar times
    threadId <- forkIO (forever' trackActions)
    trheadId2 <- forkIO (savePeriodically storage)
    readCommandsFromStdin
