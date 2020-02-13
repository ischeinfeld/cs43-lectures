module Shell1 where

import Data.List.Split

import System.Posix.Process
import System.Posix.Types
import System.Console.Readline (readline)
import System.Directory
import System.Posix.Directory
import System.Exit
import System.IO.Error
import Control.Monad

data StatusCode = Exit | Prompt | Wait deriving (Eq, Show)
data Status = Status { code :: StatusCode, pid :: Maybe ProcessID}

builtinCmds :: [(String, [String] -> IO Status)]
builtinCmds = [("help", hashHelp),
               ("cd", hashCd)]

main :: IO ()
main = prompt

prompt :: IO ()
prompt = do
  dir <- getCurrentDirectory
  putStr (last (splitOn "/" dir) ++ " $ ")
  tok <- liftM (splitOn " ") getLine
  status <- hashRun tok
  
  let responder status =
        case status of
          Status {code=Prompt} -> main
          Status {code=Exit} -> return ()
          Status {code=Wait, pid=(Just childPid)} -> wait childPid responder
  
  responder status

hashRun :: [String] -> IO Status
hashRun (cmd:args) =
  let func = lookup cmd builtinCmds
  in case func of
    Just f -> f args
    Nothing -> execute cmd args

-- Wait on child PID, handle errors, and then continue
wait :: ProcessID -> (Status -> IO ()) -> IO ()
wait childPID responder = do
  status <- getProcessStatus True False childPID
  case status of
    Nothing -> fail "Error: nothing from getProcess status"
    Just ps -> case ps of
      Exited ExitSuccess ->
        responder Status {code = Prompt, pid = Nothing}
      _ -> responder Status {code = Prompt, pid = Nothing}
  

execute :: String -> [String] -> IO Status
execute cmd args = do
  -- execvp
  pid <- forkProcess (executeFile cmd True args Nothing)
  case pid of
    0 -> exit -- child forked: exit
    -1 -> exit -- error: exit
    _ -> return Status {code = Wait, pid = Just pid} -- parent waits on child
  where exit = return Status {code = Exit, pid = Nothing}

--------- BUILTINS -------------

hashHelp :: [String] -> IO Status
hashHelp args = do
  putStrLn "--------"
  putStrLn "Welcome to our shell!"
  putStrLn "--------"
  putStrLn "Builtins:"
  mapM_ (putStrLn . fst) builtinCmds
  putStrLn ""
  return Status {code = Prompt, pid = Nothing}

hashCd :: [String] -> IO Status
hashCd [args] =
  do change <- tryIOError (changeWorkingDirectory args)
     let status = Status {code = Prompt, pid = Nothing}
     case change of
       Left e -> do putStrLn $ "cd: no such file or directory: " ++ args
                    return status
       Right retCode -> return status
hashCd _ = do putStrLn "Not a valid path..."
              return Status {code = Prompt, pid = Nothing}
