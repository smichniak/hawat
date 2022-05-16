module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.IntMap.Strict as IntMap

import AbsHawat
import InterpreterEnv
import InterpreterError

interpretProgram :: Program -> Either InterpreterError Ans
interpretProgram (ProgramL _ topDefList) = (runReader $ runExceptT $ interpretTopDefProgram topDefList) initIEnv
--interpretProgram (ProgramL _ topDefList) = (runCont (runReaderT (runExceptT (interpretTopDefs topDefList)) initIEnv)) id


-- Empty identifier is not taken, we can use it
callMain = Global BNFC'NoPosition (DeclL Nothing (Int Nothing) [Init Nothing (Ident "") (EApp Nothing (EVar Nothing (Ident "main")) [])])

interpretTopDefProgram :: [TopDef] -> IM Ans
interpretTopDefProgram defs = evalTopDefs (defs) emptyCont initStore
   -- finalCont <- evalTopDefs (defs ++ [callMain]) emptyCont
 --   finalAns <- 
    
  --  return finalAns 

 --   throwError $ IE Nothing IEDivZero


evalTopDefs :: [TopDef] -> Cont -> Store -> IM Ans
evalTopDefs [] cont store = return $ cont store
evalTopDefs (def : restDefs) cont store = do
    let envCont = \newEnv newStore -> local (const newEnv) (evalTopDefs restDefs cont newStore)
    finalAns <- evalTopDef def envCont store
    return finalAns

evalTopDef :: TopDef -> (InterpreterEnv -> Store -> IM Ans) -> Store -> IM Ans
evalTopDef (Global _ decl) envCont s = evalDecl decl envCont s
--evalTopDef (FnDef pos ident argList returnType block) cont = typecheckFunction pos ident argList returnType block cont
evalTopDef (FnDef pos ident argList returnType block) envCont s = do
    env <- ask
    envCont env s


evalDecl :: Decl -> (InterpreterEnv -> Store -> IM Ans) -> Store -> IM Ans
evalDecl (DeclL _ _ []) envCont store = do
    env <- ask
    envCont env store
 --   ans <- envCont env store

  --  return ans

evalDecl (DeclL declPos typ ((NoInit initPos ident) : xs)) envCont store = evalDecl (DeclL declPos typ ((Init initPos ident (typeDefaultValue typ)) : xs)) envCont store


--    newEnv <- declare ident typ
--    local (const newEnv) (typecheckDecl $ DeclL declPos typ xs)             


evalDecl (DeclL declPos typ ((Init initPos ident expr) : xs)) envCont store = do
    env <- ask

    let exprCont = \val -> let (newEnv, newStore) = declare ident val env store in 
            local (const newEnv) (evalDecl (DeclL declPos typ xs) envCont newStore)

      --      restDeclCont <- local (const newEnv) (evalDecl (DeclL declPos typ xs) contEnv)
          --  return ans)

    evalExpr expr exprCont store
   -- ans <- evalExpr expr exprCont store
   -- return ans


  --  newEnv <- declare ident typ
   -- local (const newEnv) (typecheckStmt (Ass initPos (EVar initPos ident) expr))
   -- local (const newEnv) (typecheckDecl $ DeclL declPos typ xs)




evalExpr :: Expr -> (StoreData -> IM Ans) -> Store -> IM Ans
evalExpr (ELitInt _ int) contExprM _ = contExprM (IntS int)
evalExpr (EVar _ ident) contExprM store = do
    env <- ask
    contExprM (getVal ident env store)
evalExpr (ELitTrue _) contExprM _ = contExprM (BoolS True)
evalExpr (ELitFalse _) contExprM _ = contExprM (BoolS False)
evalExpr (EVoid _) contExprM _ = contExprM VoidS
evalExpr (EString _ str) contExprM _ = contExprM (StringS str)



-- TODO Move to utils
typeDefaultValue :: Type -> Expr
typeDefaultValue (Int pos) = ELitInt pos 0
typeDefaultValue (Bool pos) = ELitFalse pos
typeDefaultValue (Str pos) = EString pos ""
typeDefaultValue (Void pos) = EVoid pos
typeDefaultValue (Arr pos arrType) = EArray pos []
typeDefaultValue (Fun pos typeList returnType) = ELambda pos (getPlaceholderArgs typeList) returnType (returnDefault returnType)

naturals = iterate (+1) 1
getPlaceholderArgs :: [Type] -> [Arg]
getPlaceholderArgs = zipWith (\ident t -> ArgL (hasPosition t) t (Ident $ show ident)) naturals
returnDefault returnType = BlockL BNFC'NoPosition [Ret BNFC'NoPosition (typeDefaultValue returnType)]

