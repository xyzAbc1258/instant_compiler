module JVMCompiler where

import Common
import AbsInstant
import JVMCommon
import Data.List

compile::String -> Program -> String
compile name p = let (stack, newP) = calcAndOptimizeStack p in
                   let localVars = calcLocals newP in
                    let instructionsM = generateInstructionsP newP in
                      let instructions = runGen instructionsM $ envFromList localVars in
                        let fullPrefix = concatLines [prefix name, stackSize stack, localsSize $ length localVars + 1] in
                          let content = concatLines $ map show instructions in
                            concatLines [fullPrefix, content, postfix]


prefix::String -> String
prefix name = let s = ".super java/lang/Object\n\
                      \.method public <init>()V\n\
                      \   aload_0\n\
                      \   invokespecial java/lang/Object/<init>()V\n\
                      \   return\n\
                      \.end method\n\
                      \.method public static main([Ljava/lang/String;)V\n\
                      \" in
                      concatLines [".class " ++ name, s]

stackSize::Int -> String
stackSize s = ".limit stack " ++ show s

localsSize::Int -> String
localsSize s = ".limit locals " ++ show s

postfix::String
postfix = "return\n.end method"

calcLocals::Program -> [String]
calcLocals (Prog stmts) =
  let names = [i | SAss (Ident i) _ <- stmts] in
    nub names

calcAndOptimizeStack::Program -> (Int, Program)
calcAndOptimizeStack (Prog stmts) = let pairs = map calcAndOptimizeStmt stmts in
  let (heights, nStmts) = unzip pairs in
  (maximum heights, Prog nStmts)

calcAndOptimizeStmt::Stmt -> (Int, Stmt)
calcAndOptimizeStmt (SAss i e) = let (h, ne) = calcAndOptimizeExp e in (h, SAss i ne)

calcAndOptimizeStmt (SExp e) = let (h, ne) = calcAndOptimizeExp e in (h, SExp ne)

calcAndOptimizeExp::Exp -> (Int, Exp)
calcAndOptimizeExp e@(ExpVar _) = (1, e)

calcAndOptimizeExp e@(ExpLit _) = (1, e)

calcAndOptimizeExp (ExpAdd e1 e2) =
  let (h1, o1) = calcAndOptimizeExp e1 in
    let (h2, o2) = calcAndOptimizeExp e2 in
    if h2 > h1 then (h2, ExpAdd o2 o1) else (max h1 (h2 + 1), ExpAdd o1 o2)

calcAndOptimizeExp (ExpMul e1 e2) =
  let (h1, o1) = calcAndOptimizeExp e1 in
    let (h2, o2) = calcAndOptimizeExp e2 in
    if h2 > h1 then (h2, ExpMul o2 o1) else (max h1 (h2 + 1), ExpMul o1 o2)

calcAndOptimizeExp (ExpSub e1 e2) =
  let (h1, o1) = calcAndOptimizeExp e1 in
    let (h2, o2) = calcAndOptimizeExp e2 in
      if h2 > h1 then (h2, ExpSubInv o2 o1) else (max h1 (h2 + 1), ExpSub o1 o2)

calcAndOptimizeExp (ExpDiv e1 e2) =
  let (h1, o1) = calcAndOptimizeExp e1 in
    let (h2, o2) = calcAndOptimizeExp e2 in
      if h2 > h1 then (h2, ExpDivInv o2 o1) else (max h1 (h2 + 1), ExpDiv o1 o2)

generateInstructionsP::Program -> Generator Instruction ()
generateInstructionsP (Prog s) = mapM_ generateInstructionsS s

generateInstructionsS::Stmt -> Generator Instruction ()
generateInstructionsS (SAss (Ident i) e) = do
  generateInstructionsE e
  getVal i >>= (gwrite . Store)

generateInstructionsS (SExp e) = do
  generateInstructionsE e
  gwrite Print

generateInstructionsE::Exp -> Generator Instruction ()
generateInstructionsE (ExpAdd e1 e2) = generateBinary e1 e2 [Add]
generateInstructionsE (ExpSub e1 e2) = generateBinary e1 e2 [Sub]
generateInstructionsE (ExpSubInv e1 e2) = generateBinary e1 e2 [Swap, Sub]
generateInstructionsE (ExpMul e1 e2) = generateBinary e1 e2 [Mul]
generateInstructionsE (ExpDiv e1 e2) = generateBinary e1 e2 [Div]
generateInstructionsE (ExpDivInv e1 e2) = generateBinary e1 e2 [Swap, Div]
generateInstructionsE (ExpLit e) = (gwrite . Const . fromInteger) e
generateInstructionsE (ExpVar (Ident e)) = getVal e >>= (gwrite . Load)

generateBinary::Exp -> Exp -> [Instruction] -> Generator Instruction ()
generateBinary e1 e2 i = do
  generateInstructionsE e1
  generateInstructionsE e2
  mapM_ gwrite i