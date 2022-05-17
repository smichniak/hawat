module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.IntMap.Strict as IntMap

import AbsHawat
import InterpreterEnv
import InterpreterError

interpretProgram :: Program -> Either InterpreterError Ans
interpretProgram (ProgramL _ topDefList) = evalState ((runReaderT $ runExceptT $ interpretTopDefProgram topDefList) initIEnv) initStore
--interpretProgram (ProgramL _ topDefList) = (runCont (runReaderT (runExceptT (interpretTopDefs topDefList)) initIEnv)) id


-- Empty identifier is not taken, we can use it
callMain = Global BNFC'NoPosition (DeclL Nothing (Int Nothing) [Init Nothing (Ident "") (EApp Nothing (EVar Nothing (Ident "main")) [])])

interpretTopDefProgram :: [TopDef] -> IM Ans
interpretTopDefProgram defs = do
    finalCont <- evalTopDefs (defs) emptyCont -- TODO Restore: evalTopDefs (defs ++ [callMain]) emptyCont
    get
    --return store
 --   return $ finalCont store
 --   throwError $ IE Nothing IEDivZero


evalTopDefs :: [TopDef] -> Cont -> IM Cont
evalTopDefs [] cont = return cont
  --  do
  --  store <- get
  --  return $ cont store

evalTopDefs (def : restDefs) cont = do
    let envCont = \newEnv -> local (const newEnv) (evalTopDefs restDefs cont)
    evalTopDef def envCont
    --return newCont

evalTopDef :: TopDef -> ContEnv -> IM Cont
evalTopDef (Global _ decl) envCont = evalDecl decl envCont
evalTopDef (FnDef pos ident argList returnType block) envCont = do -- evalFuntion pos ident argList returnType block envCont
    env <- ask
    envCont env


--evalFuntion :: BNFC'Position -> Ident -> [Arg] -> Type -> Block -> ContEnv -> IM Cont
--evalFuntion pos ident argList returnType block envCont = do



evalDecl :: Decl -> ContEnv -> IM Cont
evalDecl (DeclL _ _ []) envCont = do
    env <- ask
    envCont env

evalDecl (DeclL declPos typ ((NoInit initPos ident) : xs)) envCont = evalDecl (DeclL declPos typ ((Init initPos ident (typeDefaultValue typ)) : xs)) envCont


evalDecl (DeclL declPos typ ((Init initPos ident expr) : xs)) envCont = do
    env <- ask
    store <- get

    let exprCont = \val -> let (newEnv, newStore) = declare ident val env store in do
            put newStore
            local (const newEnv) (evalDecl (DeclL declPos typ xs) envCont)
    evalExpr expr exprCont



evalExpr :: Expr -> ContExpr -> IM Cont
evalExpr (ELitInt _ int) contExprM = contExprM (IntS int)
evalExpr (EVar _ ident) contExprM = do
    env <- ask
    store <- get
    contExprM (getVal ident env store)
evalExpr (ELitTrue _) contExprM = contExprM (BoolS True)
evalExpr (ELitFalse _) contExprM = contExprM (BoolS False)
evalExpr (EVoid _) contExprM = contExprM VoidS
evalExpr (EString _ str) contExprM = contExprM (StringS str)


--evalExpr (EApp _ funExpr []) contExprM = evalExpr funExpr contExprM


-- evalExpr funExpr (funCont)
-- funCont :: FunctionS -> IM Cont
-- FunctionS :: [StoreData] -> ContExpr -> IM Cont

{-

evalExpr (EApp pos funExpr argExprList) contExprM = do

 --   let restExprCont = \val -> evalExpr (EApp pos funExpr restArgs) contExprM


    newContExpr :: StoreData -> IM Cont
    newContExpr :: (FunS ([StoreData] -> ContExpr -> IM Cont)) -> IM Cont
    newContExpr (FunS fun) =

    evalExpr funExpr newContExpr

--evalExpr (Lambda pos [] returnType instructionBlock) contExprM =
evalExpr (ELambda pos argList returnType instructionBlock) contExprM =
    let fun = createFunction argList instructionBlock in contExprM (FunS fun)




createFunction :: [Arg] -> Block -> [StoreData] -> ContExpr -> IM Cont
createFunction [] block [] contExpr = evalStmt block contExpr
createFunction ((ArgL _ _ ident) : restArgs) block (val : restVals) contExpr = do
    env <- ask
    store <- get
    let (newEnv, newStore) = declare ident val env store
    put newStore
    local (const newEnv) (createFunction restArgs block restVals)



evalStmt :: Stmt -> ContExpr -> IM Cont
-}

-- EApp a (Expr' a) [Expr' a]
-- ELambda a [Arg' a] (Type' a) (Block' a)


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

