module JVMCommon(
  Instruction(Const, Store, Load, Add, Mul, Sub, Div, Print, Swap)
) where

data Instruction = Const Int | Store Int | Load Int | Add | Mul | Sub | Div | Print | Swap

instance Show Instruction where
  show (Const 0) = "iconst_0"
  show (Const 1) = "iconst_1"
  show (Const 2) = "iconst_2"
  show (Const 3) = "iconst_3"
  show (Const 4) = "iconst_4"
  show (Const (-1)) = "iconst_m1"
  show (Const n) | n >= -128 && n <= 127 = "bipush " ++ show n
  show (Const n) = "sipush " ++ show n

  show (Store 0) = "istore_0"
  show (Store 1) = "istore_1"
  show (Store 2) = "istore_2"
  show (Store 3) = "istore_3"
  show (Store n) = "istore " ++ show n

  show (Load 0) = "iload_0"
  show (Load 1) = "iload_1"
  show (Load 2) = "iload_2"
  show (Load 3) = "iload_3"
  show (Load n) = "iload " ++ show n

  show Add = "iadd"
  show Sub = "isub"
  show Mul = "imul"
  show Div = "idiv"

  show Print = "invokestatic Runtime/printInt(I)V"

  show Swap  = "swap"