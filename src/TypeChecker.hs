module Typechecker where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Data

--import AbsHawat ( Type, Ident, Program, BNFC'Position, hasPosition ) -- TODO Recover this
import AbsHawat
import TypecheckerEnv
import TypecheckerError

maybeError :: Either TypeError TypecheckEnv -> Maybe TypeError
maybeError (Left err) = Just err
maybeError (Right _) = Nothing

typecheckProgram :: Program -> Maybe TypeError -- Nothing if there were no errors
typecheckProgram (ProgramL _ topDefList) = maybeError $ (runReader $ runExceptT $ typecheckTopDefs topDefList) initTCEnv

typecheckTopDefs :: [TopDef] -> TCM TypecheckEnv
typecheckTopDefs [] = ask
typecheckTopDefs (x:xs) = do
    newEnv <- typecheckTopDef x
    local (const newEnv) (typecheckTopDefs xs)

t1 = Void (BNFC'Position 2 3) -- TODO Remove
e = TEArrIndex t1
err = TE (hasPosition t1) e 

typecheckTopDef :: TopDef -> TCM TypecheckEnv
typecheckTopDef (Global _ decl) = typecheckDecl decl
typecheckTopDef (FnDef pos ident argList returnType block) = typecheckFunction pos ident argList returnType block -- TODO

getItemIdent :: Item -> Ident
getItemIdent (NoInit _ ident) = ident
getItemIdent (Init _ ident _) = ident

typecheckDecl :: Decl -> TCM TypecheckEnv
typecheckDecl (DeclL declPos typ []) = ask
typecheckDecl (DeclL declPos typ (item : xs)) = do -- TODO Add checking type initialization expression
    newEnv <- declare (getItemIdent item) typ
    local (const newEnv) (typecheckDecl $ DeclL declPos typ xs)

getArgType :: Arg -> Type
getArgType (ArgL argPos typ ident) = typ

functionType :: BNFC'Position -> [Arg] -> Type -> Type
functionType pos argList returnType = Fun pos (map getArgType argList) returnType

typecheckFunction :: BNFC'Position -> Ident -> [Arg] -> Type -> Block -> TCM TypecheckEnv
typecheckFunction pos ident argList returnType block = do
    newEnv <- declare ident (functionType pos argList returnType)
    argEnv <- local (addLevel . const newEnv) (declareArgs argList)
    local (setReturnType returnType . const argEnv) (typecheckBlock block)
    return newEnv

typecheckBlock :: Block -> TCM TypecheckEnv
typecheckBlock (BlockL _ []) = ask
typecheckBlock (BlockL blockPos (stmt : xs)) = do
    newEnv <- typecheckStmt stmt
    local (const newEnv) (typecheckBlock $ BlockL blockPos xs)

compareTypes :: Type -> Type -> Bool
compareTypes _ _ = False -- TODO fix
--compareTypes t1 t2 = Data.Data.toConstr t1 == Data.Data.toConstr t2

typecheckStmt :: Stmt -> TCM TypecheckEnv
typecheckStmt (Empty _) = ask
typecheckStmt (BStmt _ block) = do
    env <- ask
    local (addLevel . const env) (typecheckBlock block)
typecheckStmt (SDecl _ decl) = typecheckDecl decl
typecheckStmt (Ass _ mut expr) = do
    mutType <- typecheckMut mut
    exprType <- typecheckExpr expr
    case compareTypes mutType exprType of
        False -> throwError $ makeError mutType (TEAssignment mutType exprType)
        True -> do
            env <- ask
            return env
        
        

typecheckExpr :: Expr -> TCM Type
typecheckExpr (EVar pos ident) = getType pos ident     
typecheckExpr (ELitTrue pos) = do return $ Bool pos
typecheckExpr (ELitFalse pos) = do return $ Bool pos
typecheckExpr (EVoid pos) = do return $ Void pos
--typecheckExpr (EApp pos exprList) = 

typecheckMut :: Mut -> TCM Type
typecheckMut (MutL pos ident) = getType pos ident -- TODO Array mut
