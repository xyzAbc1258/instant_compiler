module Common(
  Generator,
  GeneralGenerator,
  gwrite,
  getVal,
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

type Env a = M.Map String a
type GeneralGenerator s a = WriterT (Endo [a]) (State (Env s))

type Generator a = GeneralGenerator Int a

runGen::GeneralGenerator s a b -> Env s -> [a]
runGen g e = appEndo (evalState (execWriterT g) e) []

emptyEnv::Env s
emptyEnv = M.empty

envFromList::[String] -> Env Int
envFromList l = M.fromList $ zip l [1 .. (length l)]

gwrite::a -> GeneralGenerator s a ()
gwrite l = tell $ Endo ([l] ++)

getVal::String -> GeneralGenerator s a s
getVal s = do
  st <- get
  let num = st M.! s
  return num

newline::String
newline = "\n"

concatLines::[String] -> String
concatLines = foldl1 (\a b -> a ++ newline ++ b)