module LLVMCommon(
  Value(Const, Var),
  Instruction(Assign, Print),
  Op,
  LLVMIdent(LLVMIdent),
  addOp, subOp, mulOp, divOp, val,
  getCurrent, getNext
) where

import Common
import Control.Monad.State
import GHC.Base
import qualified Data.Map as M

data LLVMIdent = LLVMIdent String

data Value = Const Int | Var LLVMIdent
data OpType = Add | Sub | Mul | Div
data Op = Op OpType Value Value | Val Value
data Instruction = Assign LLVMIdent Op | Print Value

addOp = Op Add
subOp = Op Sub
mulOp = Op Mul
divOp = Op Div
val = Val

instance Show LLVMIdent where
  show(LLVMIdent i) = "%" ++ i

instance Show Value where
  show (Const i) = show i
  show (Var s) = show s

instance Show OpType where
  show Add = "add"
  show Sub = "sub"
  show Mul = "mul"
  show Div = "div"

instance Show Op where
  show(Op op v1 v2) = show op ++ " i32 " ++ show v1 ++ ", " ++ show v2
  show (Val v) = show v

instance Show Instruction where
  show (Assign i o) = show i ++ " = " ++ show o
  show (Print i) = "call .... sth(i32 " ++ show i ++ ")"

getCurrent::String -> Generator a Value
getCurrent a = do
  n <- getNum a
  return $ Var $ LLVMIdent $ a ++ "_" ++ show n

getNext::String -> Generator a LLVMIdent
getNext a = do
  modify (M.alter (\v -> fmap (+ 1) v <|> Just 0 ) a)
  (\(Var a) -> a) <$> getCurrent a