module Gy07 where

import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Control.Monad.State
import Data.List
import Control.Monad


-- Cheatsheet:
{-
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| Monad                     | Primitive Function #1                       | Primitive Function #2                                       |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| State s a                 | get :: State s s                            | put :: s -> State s ()                                      |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| Monoid w => Writer w a    |                                             | tell :: w -> Writer w ()                                    |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| Reader r a                | ask :: Reader r r                           | local :: (r -> r) -> Reader r a -> Reader r a               |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
| Except e a                | throwError :: e -> Except e a               | catchError :: Except e a -> (e -> Except e a) -> Except e a |
+---------------------------+---------------------------------------------+-------------------------------------------------------------+
-}

-- These side effects are not very useful on their own
-- They would be stronger if we could use more of them at once
-- This technology is called "Monad Transformers"
-- We will change the four known monads in the following way
{-

newtype State  s a = State  { runState  :: s -> (a,s) } ==> newtype StateT  s m a = StateT  { runStateT  :: s -> m (a, s) }
newtype Reader r a = Reader { runReader :: r -> a }     ==> newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a  }
newtype Writer w a = Writer { runWriter :: (a, w) }     ==> newtype WriterT s m a = WriterT { runWriterT :: m (a, w) }
newtype Except e a = Except { runExcept :: Either e a } ==> newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

We wrap the result in an arbitrary monad everywhere, so we can nest the side effects
The primitive operations in the standard library also find nested monads
-}

-- Take the env type from a previous practice
data Env = MkEnv { isAdmin :: Bool, homeDir :: String } deriving (Eq, Show)

-- Define a function which tells the users home directory if they are an admin.
printHomeDirIfAdmin :: ReaderT Env (Writer [String]) ()
printHomeDirIfAdmin = do
  env <- ask
  if isAdmin env then tell [homeDir env] else return ()

-- To run:
-- runWriter (runReaderT printHomeDirIfAdmin (MkEnv True "/home/stefania"))

-- Why do tell, ask, etc. typecheck?
-- :t ask
-- Every transformer has a type class, which has an instance if the monad stack contains the transformer
-- These type classes can also be used to define functions

printHomeDirIfAdmin' :: (MonadReader Env m, MonadWriter [String] m) => m ()
printHomeDirIfAdmin' = do
  env <- ask
  if isAdmin env then tell [homeDir env] else return ()

-- In the exam, everyone will have to write down their own type signature for this task.
-- Both can be used.

-- Let's take the following type synonym
type FileSystem = [String]
-- ex.:
basicExecutables :: FileSystem
basicExecutables = ["/usr/bin/bash", "/usr/bin/ls", "/bin/sh"]

-- Task:
-- Let's have a FileSystem type state environment
-- Let's have an Env type reader environment
-- Let's put the user's home directory into the file system if it is not already there
-- Hint: use elem
addHomeIfNotIn :: StateT FileSystem (Reader Env) ()
addHomeIfNotIn = do
  files <- get -- State
  user <- ask -- Reader
  if elem (homeDir user) files
  then return ()
  else do
    put $ (homeDir user) : files

-- To run:
-- runReader (runStateT addHomeIfNotIn basicExecutables) (MkEnv True "/bin/sh")

-- Let's have a FileSystem type state environment
-- Let's have a [String] type writer environment
-- Let's delete duplicate files from the file system and write them to the writer environment
-- Hint: use nub, (\\)
undupe :: StateT FileSystem (Writer [String]) ()
--undupe :: (MonadState FileSystem m, MonadWriter [String] m) => m ()
undupe = do
  files <- get -- State
  tell (files \\ nub files) -- Writer
  put (nub files) -- State

-- To run:
-- runWriter (runStateT undupe ["a", "a", "b", "c", "c", "ddd"])

data FSError = FileExists | NotAnAdmin | BadPath deriving (Eq, Show)
-- Let's have a FileSystem type state environment
-- Let's have an FSError type except environment
-- Let's have an Env type reader environment
-- The function takes a filepath as a parameter. 
-- If the user is not an administrator, throw a NotAnAdmin error, and if the file exists, throw a FileExists error
-- If it does not exist, put it in the file system
tryAdd :: String -> StateT FileSystem (ExceptT FSError (Reader Env)) ()
tryAdd path = do
  files <- get -- State
  user <- ask -- Reader
  if (not (isAdmin user))
  then throwError NotAnAdmin
  else if (elem path files)
  then throwError FileExists
  else put (path : files)

-- To run:
-- runReader (runExceptT (runStateT (tryAdd "/bin/sh") basicExecutables)) (MkEnv True "/home/stef")  

-- watch out with ExceptT!!!
what :: (MonadError String m, MonadWriter [String] m) => m ()
what = do
  tell ["Will this be printed?"]
  throwError "Error"
  tell ["What about this?"]

-- The order of the stack matters
-- The exam will only include monad stacks in which Except is the outermost layer, so if there is an error, everything else will crash and burn


-- IO in the stack
-- The IO monad can also be added to the stack, but only at the bottom
-- Since every transformer takes a monad as a parameter, IO can also be added
getAndPrint :: WriterT [String] IO ()
getAndPrint = do
  input <- liftIO getLine
  tell [input]
  -- x <- getLine does not type check, because getLine :: IO String
  -- there is a function called liftIO that can run an IO function in a transformer

-- To run:
-- runWriterT getAndPrint

-- :t lifIO :: MonadIO m => IO a -> m a
getAndPrint' :: (MonadWriter [String] m, MonadIO m) => m ()
getAndPrint' = do
  input <- liftIO getLine
  tell [input]

-- Essentially, MonadX m means that m is a Monad that also has X capabilities

-- In the next task, we will work with the following environment
-- Env type reader environment
-- FSError type except environment
-- FileSystem type state environment
-- [String] type writer environment
-- IO environment
-- Usually, only 4 out of 5 will be on the exam

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x = map tail . groupBy (/=) . (x:)

-- Read a string from the console
-- If it is a valid path (non-empty strings separated by /), add it to the file system
-- If not, throw a BadPath error
f1 :: ReaderT Env (ExceptT FSError (StateT FileSystem (WriterT [String] IO))) ()
f1 = do
  str <- liftIO getLine
  if any null (splitOn '/' str) -- invalid path
  then throwError BadPath
  else do
    files <- get
    put (str : files)

-- Run f1
-- If it throws an error, we write this in the writer environment, indicating that it failed, then try f1 again
-- As a result, we return the number of attempts it took for the user to provide a valid path
f2 :: ReaderT Env (ExceptT FSError (StateT FileSystem (WriterT [String] IO))) Int
f2 = do
  catchError (f1 >> return 0) (\e -> do
    tell ["Threw an error"]
    n <- f2
    return $ n+1
    )

-- Go through all the items in the file system
-- Ask the user (i.e., prompt them on the console) if they want to keep that file
-- If they answer yes or it is the user's home directory, leave it in the list; if not, delete it
-- Move the deleted files to the writer environment
-- Hint: use filterM
f3 :: ReaderT Env (ExceptT FSError (StateT FileSystem (WriterT [String] IO))) ()
f3 = do
  files <- get
  new_files <- filterM (\file -> do
    liftIO $ putStrLn ("Do you want to keep file " ++ file ++ "? Type Y or N:")
    input <- liftIO getLine
    readerEnv <- ask 
    if homeDir readerEnv == file then return True else
      case input of
        "Y" -> return True
        _ -> tell [file] >> return False -- Move deleted files to writer
    ) files 
  put new_files -- Updated list of files


-- Other notable transformers
-- ContT
-- AccumT
