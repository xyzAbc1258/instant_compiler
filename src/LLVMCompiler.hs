module LLVMCompiler where

import Common
import LLVMCommon
import AbsInstant
import Data.List


compile::Program -> String
compile p =  let instructionsM = generateInstructionsP p in
                let instructions = runGen instructionsM emptyEnv in
                   let content = concatLines $ map show instructions in
                      concatLines [prefix, content, postfix]


prefix::String
prefix = "define i32 @main() {"

postfix::String
postfix = "ret i32 0\n}"

generateInstructionsP::Program -> Generator Instruction ()
generateInstructionsP (Prog s) = mapM_ generateInstructionsS s

generateInstructionsS::Stmt -> Generator Instruction ()
generateInstructionsS (SAss (Ident i) e) = do
  v <- generateInstructionsE e
  next <- getNext i
  gwrite $ Assign next $ val v

generateInstructionsS (SExp e) = do
  v <- generateInstructionsE e
  gwrite $ Print v

generateInstructionsE::Exp -> Generator Instruction Value
generateInstructionsE (ExpAdd e1 e2) = generateBinary e1 e2 addOp
generateInstructionsE (ExpSub e1 e2) = generateBinary e1 e2 subOp
generateInstructionsE (ExpMul e1 e2) = generateBinary e1 e2 mulOp
generateInstructionsE (ExpDiv e1 e2) = generateBinary e1 e2 divOp
generateInstructionsE (ExpLit e) = return $ Const $ fromInteger e
generateInstructionsE (ExpVar (Ident e)) = getCurrent e

generateBinary::Exp -> Exp -> (Value -> Value -> Op) -> Generator Instruction Value
generateBinary e1 e2 op = do
  v1 <- generateInstructionsE e1
  v2 <- generateInstructionsE e2
  tmpName <- getNext "tmp"
  gwrite $ Assign tmpName $ op v1 v2
  return $ Var tmpName
