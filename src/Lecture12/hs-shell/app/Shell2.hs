module Shell2 where

import Data.List
import Data.List.Utils (replace)
import Data.List.Split
import Data.Text (strip, unpack, pack)

import System.Posix.Process
import System.Posix.Types
import System.Console.Readline (readline, addHistory)
import System.Directory
import System.Posix.Directory
import System.Exit
import System.IO.Error
import Control.Monad
import System.Environment

data StatusCode = Exit | Prompt | Wait deriving (Eq, Show)
data Status = Status { code :: StatusCode, pid :: Maybe ProcessID}

builtinCmds :: [(String, [String] -> IO Status)]
builtinCmds = [("help", hashHelp),
               ("cd", hashCd),
               ("exit", hashExit),
               ("export", hashExport),
               ("printenv", hashPrintenv)]

main :: IO ()
main = prompt

prompt :: IO ()
prompt = do
  dir <- getCurrentDirectory
  input <- readline $ last (splitOn "/" dir) ++ " $ "
  case input of
    Nothing -> prompt
    Just i -> runCommand i

runCommand :: String -> IO ()
runCommand line = do
  addHistory line 
  env <- getEnvironment
  let tok = (splitOn " " . replaceEnvVars env . unpack . strip . pack) line
  status <- hashRun tok

  let responder status =
        case status of
          Status {code=Prompt} -> main
          Status {code=Exit} -> return ()
          Status {code=Wait, pid=(Just childPid)} -> wait childPid responder
  
  responder status

replaceEnvVars :: [(String, String)] -> String -> String
replaceEnvVars env x = 
  foldl (\acc xs ->
    if ("$" ++ fst xs) `isInfixOf` acc
      then replace ("$" ++ fst xs) (snd xs) acc
      else acc) x env

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
    Just ps -> responder Status {code = Prompt, pid = Nothing} 

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

hashPrintenv :: [String] -> IO Status
hashPrintenv [] = do env <- getEnvironment
                     mapM_ (\x -> putStrLn $ fst x ++ "=" ++ snd x) env
                     return Status {code = Prompt, pid = Nothing}
hashPrintenv _ = do putStrLn "Too many arguments..."
                    return Status {code = Prompt, pid = Nothing}

hashExport :: [String] -> IO Status
hashExport [arg] = do let (x:y:_) = splitOn "=" arg
                      setEnv x y
                      return Status {code = Prompt, pid = Nothing}
hashExport _ = do putStrLn "Wrong number of args..."
                  return Status {code = Prompt, pid = Nothing}

hashExit :: [String] -> IO Status
hashExit _ = do
  putStrLn "Exiting..."
  return Status {code = Exit, pid = Nothing}
