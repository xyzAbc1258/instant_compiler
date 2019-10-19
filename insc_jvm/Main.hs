module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import LexInstant
import ParInstant
import SkelInstant
import PrintInstant
import AbsInstant
import ErrM
import qualified JVMCompiler as JVM
import System.Exit
import System.FilePath
import System.Process
import System.Environment

type ParseFun a = [Token] -> Err a

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= (\c -> run p c f)

run :: ParseFun Program -> String -> String -> IO ()
run p s f = let ts = myLexer s in case p ts of
           Bad err    -> do putStrLn "\nERROR\n"
                            putStrLn err
                            exitWith $ ExitFailure 1
           Ok  tree -> compileAndSaveTree f tree >>= compile




compileAndSaveTree :: String -> Program -> IO String
compileAndSaveTree s tree
 = do
      let jvm = JVM.compile (takeFileName (dropExtension s)) tree
      let f = dropExtension s
      let jFile = addExtension f "j"
      writeFile jFile jvm
      return jFile

compile::String -> IO()
compile file = do
  let dir = takeDirectory file
  execDir <- takeDirectory <$> getExecutablePath
  let jasminJarPath = execDir </> "lib" </> "jasmin.jar"
  let shellCommand = "java -jar " ++ jasminJarPath ++ " "  ++ file ++ " -d " ++ dir
  callCommand shellCommand

main :: IO ()
main = do args <- getArgs
          mapM_ (runFile pProgram) args