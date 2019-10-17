module LLVMCommon(
  Value(Const, Var),
  Instruction(Assign, Print),
  Op,
  LLVMIdent(LLVMIdent),
  LLVMGenerator,
  addOp, subOp, mulOp, divOp, val,
  getCurrent, getNext, updateValue
) where

import Common
import Control.Monad.State
import GHC.Base
import qualified Data.Map as M

type LLVMGenerator a = GeneralGenerator Value a

data LLVMIdent = LLVMIdent String Int

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
  show(LLVMIdent i v) = "%" ++ i ++ "_" ++ show v

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
  show (Print i) = "call void @printInt(i32 " ++ show i ++ ")"

getCurrent::String -> LLVMGenerator a Value
getCurrent = getVal

nextIdent::String -> Value -> Value
nextIdent _ (Var (LLVMIdent n i)) = Var $ LLVMIdent n $ i +1
nextIdent a (Const _) = Var $ LLVMIdent a 0

getNext::String -> LLVMGenerator a LLVMIdent
getNext a = do
  current <- getVal a
  modify (M.alter (\v -> fmap (nextIdent a) v <|> (Just $ Var $ LLVMIdent a 0) ) a)
  (\(Var x) -> x) <$> getVal a

updateValue::String -> Value -> LLVMGenerator a ()
updateValue a v = modify(M.insert a v)