{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Main where

-- example modified from blogpost by Josh Clayton

import           Control.Monad
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Except as Except 
import qualified Control.Monad.Trans.Reader as Reader
import           Control.Exception (IOException)
import qualified Control.Exception as E

import qualified Data.Bool as B
import qualified Data.Char as C
import           Options.Applicative

-- newtype ExceptT e m a = ExceptT (m (Either e a))
-- newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

--------------------------------------------
-- transformers allow lifting monadic values
--------------------------------------------

class MonadTrans t where
    lift :: (Monad m) => m a -> t m a

instance MonadTrans (ExceptT e) where
    lift = Except.ExceptT . liftM Right

instance MonadTrans (ReaderT r) where
    lift = Reader.ReaderT . const


--------------------------------------------
-- a class for monads with IO
--------------------------------------------

class (Monad m) => MonadIO m where
  liftIO :: IO a -> m a

  {-
   do action1
      action2
      liftIO $ putStrLn "hi"
      action3
  -}

instance MonadIO IO where
  liftIO = id

instance (MonadIO m) => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO


--------------------------------------------
-- a class for monads that can throw errors
--------------------------------------------

class (Monad m) => MonadError e m | m -> e where
  throwError :: e -> m a

  catchError :: m a -> (e -> m a) -> m a

  {-
   data Error = ErrorVal1 | ErrorVal2

   do v <- action1
      catchError
        do v <- action1
           when (pred v) (throwError ErrorVal1)
           v2 <- action2
           when (pred2 v2) (throwError ErrorVal2) 
           action3
        \case 
           ErrorVal1 -> action3' -- must have same type as action3
           ErrorVal2 -> action3''
        
  -}

instance Monad m => MonadError e (ExceptT e m) where
    throwError = Except.throwE
    catchError = Except.catchE

instance MonadError e m => MonadError e (ReaderT r m) where
    throwError = lift . throwError
    catchError = Reader.liftCatch catchError


-------------------------------------------------------
-- a class for monads that can read from an environment
-------------------------------------------------------

class Monad m => MonadReader r m | m -> r where
  ask :: m r
  ask = reader id

  reader :: (r -> a) -> m a
  reader f = do
    r <- ask
    return (f r)

  local :: (r -> r) -> m a -> m a

  {-
   do
     config <- ask
     val <- reader getValFromConfig
     local (modifyVal $ val + 1) $ do
       config <- ask
       ...
  -}

instance Monad m => MonadReader r (ReaderT r m) where
    ask = Reader.ask
    local = Reader.local
    reader = Reader.reader

instance MonadReader r m => MonadReader r (ExceptT e m) where
    ask   = lift ask
    local = Except.mapExceptT . local
    reader = lift . reader


-----------------------------------------------
-- now we can write our app!
-----------------------------------------------

data Options = Options
    { oCapitalize :: Bool 
    , oExcited :: Bool
    , oStdIn :: Bool
    , oFileToRead :: Maybe String }
  deriving (Show)


newtype App a = App {
  runApp :: ReaderT Options (ExceptT IOException IO) a
} deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Options
           , MonadError IOException
           , MonadIO )


main :: IO ()
main = parseCLI >>= (runProgram app)

-- > :main --file "testfile.txt" 
-- > :main --file "testfile.txt" --excited
-- > :main --file "testfile.txt" --capitalize
-- > :main --file "testfile.txt" --capitalize --excited


-----------------------------------------------
-- CLI parsing
-----------------------------------------------

parseCLI :: IO Options
parseCLI = execParser (withInfo parseOptions "File Fun")
  where
    withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Parser Options
parseOptions = Options
    <$> (switch $ long "capitalize")
    <*> (switch $ long "excited")
    <*> (switch $ long "stdin")
    <*> (optional $ strOption $ long "file")


-----------------------------------------------
-- Program
-----------------------------------------------

runProgram :: App () -> Options -> IO ()    -- app :: App ()
runProgram app o = let rt = runApp app             :: ReaderT Options (ExceptT IOException IO) ()
                       et = Reader.runReaderT rt o :: ExceptT IOException IO ()
                       io = Except.runExceptT et   :: IO (Either IOException ())
                    in either renderError return =<< io
  where renderError e = do
          putStrLn "There was an error:"
          putStrLn $ "  " ++ show e

app :: App ()               -- do
app = getSource             --   source <- getSource
  >>= handleCapitalization  --   capitalized <- handleCapitalization source
  >>= handleExcitedness     --   excited <- handleExcitedness capitalized
  >>= (liftIO . putStr)     --   liftIO $ putStr excited

-- :t getSource
-- :t getSource @App


-----------------------------------------------
-- data retrieval
-----------------------------------------------

getSource
  :: ( MonadReader Options m
     , MonadError IOException m
     , MonadIO m )
  => m String

getSource = do   
  stdInFlag <- reader oStdIn
  if stdInFlag
    then liftIO getContents
    else loadContents

-- getSource' = B.bool loadContents (liftIO getContents) =<< reader oStdIn


loadContents
  :: ( MonadReader Options m
     , MonadIO m
     , MonadError IOException m ) 
  => m String

loadContents = maybe defaultResponse readFileFromOptions =<< reader oFileToRead
  where
    readFileFromOptions f = either throwError return =<< liftIO (safeReadFile f) -- once BF.first IOError <$> liftIO (safeReadFile f)
    defaultResponse = return "This is fun!"

-----------------------------------------------
-- data transformation
-----------------------------------------------

handleCapitalization :: MonadReader Options m => String -> m String
handleCapitalization s = B.bool s (map C.toUpper s) <$> reader oCapitalize

handleExcitedness :: MonadReader Options m => String -> m String
handleExcitedness s = B.bool s ("OMG " ++ s) <$> reader oExcited


-----------------------------------------------
-- safer reading of files
-----------------------------------------------

safeReadFile :: FilePath -> IO (Either IOException String)
safeReadFile = E.try . readFile
