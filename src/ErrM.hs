-- BNF Converter: Error Monad
-- Copyright (C) 2004  Author:  Aarne Ranta

-- This file comes with NO WARRANTY and may be used FOR ANY PURPOSE.
module ErrM where

-- the Error monad: like Maybe type with error msgs

import Control.Monad (MonadPlus(..), liftM)
import GHC.Base(Alternative(..))

data Err a = Ok a | Bad String
  deriving (Read, Show, Eq, Ord)

instance Applicative Err where
  pure = Ok
  (Ok f) <*> (Ok a) = Ok $ f a
  (Bad s) <*> _ = Bad s
  _ <*> (Bad s) = Bad s
  
instance Alternative Err where
  empty = Bad "empty"
  a@(Ok _) <|> _ = a
  _ <|> a@(Ok _) = a
  b <|> _ = b

instance Monad Err where
  return      = Ok
  fail        = Bad
  Ok a  >>= f = f a
  Bad s >>= f = Bad s

instance Functor Err where
  fmap = liftM

instance MonadPlus Err where
  mzero = Bad "Err.mzero"
  mplus (Bad _) y = y
  mplus x       _ = x
