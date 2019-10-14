module Common(
  Generator,
  gwrite,
  getNum,
  runGen,
  emptyEnv,
  envFromList,
  newline,
  concatLines
) where

import Control.Monad.Writer
import Control.Monad.State
import qualified Data.Map as M
import Data.Monoid

type Env = M.Map String Int
type Generator a = WriterT (Endo [a]) (State Env)

runGen::Generator a b -> Env -> [a]
runGen g e = appEndo (evalState (execWriterT g) e) []

emptyEnv::Env
emptyEnv = M.empty

envFromList::[String] -> Env
envFromList l = M.fromList $ zip l [1 .. (length l)]

gwrite::a -> Generator a ()
gwrite l = tell $ Endo ([l] ++)

getNum::String -> Generator a Int
getNum s = do
  st <- get
  let num = st M.! s
  return num

newline::String
newline = "\n"

concatLines::[String] -> String
concatLines = foldl1 (\a b -> a ++ newline ++ b)