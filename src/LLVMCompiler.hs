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
prefix = "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n\
          \declare i32 @printf(i8*, ...)\n\
          \define void @printInt(i32 %x) {\n\
          \        %t0 = getelementptr [4 x i8], [4 x i8]* @dnl, i32 0, i32 0\n\
          \        call i32 (i8*, ...) @printf(i8* %t0, i32 %x)\n\
          \        ret void\n\
          \}\n\
          \define i32 @main() {"

postfix::String
postfix = "ret i32 0\n}"

generateInstructionsP::Program -> LLVMGenerator Instruction ()
generateInstructionsP (Prog s) = mapM_ generateInstructionsS s

generateInstructionsS::Stmt -> LLVMGenerator Instruction ()
generateInstructionsS (SAss (Ident i) e) = do
  v <- generateInstructionsE e
  updateValue i v

generateInstructionsS (SExp e) = do
  v <- generateInstructionsE e
  gwrite $ Print v

generateInstructionsE::Exp -> LLVMGenerator Instruction Value
generateInstructionsE (ExpAdd e1 e2) = generateBinary e1 e2 addOp
generateInstructionsE (ExpSub e1 e2) = generateBinary e1 e2 subOp
generateInstructionsE (ExpMul e1 e2) = generateBinary e1 e2 mulOp
generateInstructionsE (ExpDiv e1 e2) = generateBinary e1 e2 divOp
generateInstructionsE (ExpLit e) = return $ Const $ fromInteger e
generateInstructionsE (ExpVar (Ident e)) = getCurrent e

generateBinary::Exp -> Exp -> (Value -> Value -> Op) -> LLVMGenerator Instruction Value
generateBinary e1 e2 op = do
  v1 <- generateInstructionsE e1
  v2 <- generateInstructionsE e2
  tmpName <- getNext "tmp"
  gwrite $ Assign tmpName $ op v1 v2
  return $ Var tmpName
