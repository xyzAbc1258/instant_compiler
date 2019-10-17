module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import LexInstant
import ParInstant
import SkelInstant
import PrintInstant
import AbsInstant
import ErrM
import qualified LLVMCompiler as LLVM
import System.Exit
import System.FilePath

type ParseFun a = [Token] -> Err a

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= (\c -> run p c f)

run :: ParseFun Program -> String -> String -> IO ()
run p s f = let ts = myLexer s in case p ts of
           Bad err    -> do putStrLn "\nERROR\n"
                            putStrLn err
                            exitWith $ ExitFailure 1
           Ok  tree -> compileAndSaveTree f tree




compileAndSaveTree :: String -> Program -> IO ()
compileAndSaveTree s tree
 = do
      let llvm = LLVM.compile tree
      let f = dropExtension s
      let llFile = addExtension f "ll"
      writeFile llFile llvm

main :: IO ()
main = do args <- getArgs
          mapM_ (runFile pProgram) args