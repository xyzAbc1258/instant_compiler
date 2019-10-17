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
import qualified LLVMCompiler as LLVM
import System.Exit
import System.FilePath

type ParseFun a = [Token] -> Err a

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= (\c -> run v p c f)

run ::  Verbosity -> ParseFun Program -> String -> String -> IO ()
run v p s f = let ts = myLexer s in case p ts of
           Bad err    -> do putStrLn "\nERROR\n"
                            putStrLn err
                            exitWith $ ExitFailure 1
           Ok  tree -> compileAndSaveTree f tree




compileAndSaveTree :: String -> Program -> IO ()
compileAndSaveTree s tree
 = do
      let jvm = JVM.compile "Sample" tree
      let llvm = LLVM.compile tree
      let f = dropExtension s
      let jFile = addExtension f "j"
      let llFile = addExtension f "ll" 
      writeFile jFile jvm
      writeFile llFile llvm

main :: IO ()
main = do args <- getArgs
          mapM_ (runFile 2 pProgram) args
