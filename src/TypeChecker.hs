{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Typechecker where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Data (Typeable, Data, toConstr)

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

typecheckTopDef :: TopDef -> TCM TypecheckEnv
typecheckTopDef (Global _ decl) = typecheckDecl decl
typecheckTopDef (FnDef pos ident argList returnType block) = typecheckFunction pos ident argList returnType block -- TODO

typecheckDecl :: Decl -> TCM TypecheckEnv
typecheckDecl (DeclL _ _ []) = ask

typecheckDecl (DeclL declPos typ ((NoInit _ ident) : xs)) = do
    newEnv <- declare ident typ
    local (const newEnv) (typecheckDecl $ DeclL declPos typ xs)             

typecheckDecl (DeclL declPos typ ((Init initPos ident expr) : xs)) = do
    newEnv <- declare ident typ
    local (const newEnv) (typecheckStmt (Ass initPos (MutL initPos ident) expr))
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

typecheckStmt :: Stmt -> TCM TypecheckEnv
typecheckStmt (Empty _) = ask
typecheckStmt (BStmt _ block) = do
    env <- ask
    local (addLevel . const env) (typecheckBlock block)
    ask
typecheckStmt (SDecl _ decl) = typecheckDecl decl
typecheckStmt (Ass pos mut expr) = do
    mutType <- typecheckMut mut
    exprType <- typecheckExpr expr
    unless (compareTypes mutType exprType) (throwError $ makeError expr (TEAssignment mutType exprType))
    ask
typecheckStmt (Incr pos mut) = do
    mutType <- typecheckMut mut
    case mutType of
        Int _ -> ask
        t -> throwError $ TE pos (TEUnaryOp t)
typecheckStmt (Decr pos mut) = typecheckStmt (Incr pos mut)
typecheckStmt (Ret _ returnExpr) = do
    env <- ask
    let expectedType = tceReturnType env
    retType <- typecheckExpr returnExpr
    unless (compareTypes expectedType retType) (throwError $ makeError returnExpr (TEReturn retType expectedType))
    return env
typecheckStmt (VRet pos) = typecheckStmt (Ret pos (EVoid Nothing))
typecheckStmt (SCond _ cond) = do
    env <- ask
    local (addLevel . const env) (typecheckSCond cond)
    ask
typecheckStmt (While pos condExpr block) = do
    env <- ask
    local (goInsideLoop . addLevel . const env) (typecheckSCond (If pos condExpr block))
    ask
typecheckStmt (For pos ident startExpr finalExpr block) = do
    env <- ask
    startType <- typecheckExpr startExpr
    unless (compareTypes startType int)  (throwError $ makeError startExpr (TEForIter startType))
    endType <- typecheckExpr finalExpr
    unless (compareTypes endType int)  (throwError $ makeError finalExpr (TEForIter endType))
    loopEnv <- local (addLevel . const env) (declare ident (Int pos))
    local ((addReadOnly ident) . goInsideLoop . addLevel . const loopEnv) (typecheckSCond (If pos (ELitTrue pos) block))
    ask
typecheckStmt (LoopJmp _ loopS) = checkInLoop loopS
typecheckStmt (SExp pos expr) = do
    typecheckExpr expr
    ask

typecheckSCond :: Cond -> TCM TypecheckEnv
typecheckSCond (If _ condExpr block) = do
    condType <- typecheckExpr condExpr
    unless (compareTypes condType bool) (throwError $ makeError condExpr (TECondtion condType))
    typecheckBlock block
typecheckSCond (IfElse pos condExpr block1 block2) = do
    typecheckSCond (If pos condExpr block1)
    typecheckBlock block2
typecheckSCond (ElseIf pos condExpr block alternativeCond) = do
    typecheckSCond (If pos condExpr block)
    typecheckSCond alternativeCond

typecheckExpr :: Expr -> TCM Type
typecheckExpr (EVar pos ident) = getType pos ident
typecheckExpr (ELitInt pos _) = do return $ Int pos
typecheckExpr (ELitTrue pos) = do return $ Bool pos
typecheckExpr (ELitFalse pos) = do return $ Bool pos
typecheckExpr (EVoid pos) = do return $ Void pos
typecheckExpr (EApp pos ident exprList) = do
    appliedTypes <- sequence $ map typecheckExpr exprList
    funType <- getType pos ident
    case funType of 
        Fun funPos argumentList returnType ->
            case and $ (length appliedTypes == length argumentList) : zipWith compareTypes appliedTypes argumentList of -- TODO Use compare tyes on functions, change to unless
                False ->  throwError $ TE pos (TEApplication appliedTypes argumentList)
                True -> return returnType
        _ -> throwError $ TE pos (TENotFunction ident)

typecheckExpr (EGet pos ident indexExpr) = do
    containerType <- getType pos ident
    indexType <- typecheckExpr indexExpr
    case containerType of
        Arr _ arrType -> case indexType of
            Int _ -> return arrType
            typ -> throwError $ TE pos (TEArrIndex typ)
        _ -> throwError $ TE pos (TENotArray ident)

typecheckExpr (EString pos _) = do return $ Str pos
typecheckExpr (ELambda pos argList returnType block) = do
    typecheckFunction pos (Ident "") argList returnType block
    return $ functionType pos argList returnType

typecheckExpr (EArray pos [expr]) = do 
    exprType <- typecheckExpr expr
    return $ Arr pos exprType
typecheckExpr (EArray pos (expr : exps)) = do 
    restArr <- typecheckExpr (EArray pos exps)
    exprType <- typecheckExpr expr
    let Arr _ restArrType = restArr -- Pattern always fits
    unless (compareTypes exprType restArrType) (throwError $ TE pos (TEArrayType exprType restArrType))
    return $ Arr pos exprType
        

typecheckExpr (Neg pos expr) = unaryCheck pos expr negTypes
typecheckExpr (Not pos expr) = unaryCheck pos expr notTypes
typecheckExpr (EMul pos expr1 _ expr2) = binaryCheck pos expr1 expr2 mulTypes
typecheckExpr (EAdd pos expr1 _ expr2) = binaryCheck pos expr1 expr2 addTypes
typecheckExpr (ERel pos expr1 _ expr2) = binaryCheck pos expr1 expr2 relTypes
typecheckExpr (EAnd pos expr1 expr2) = binaryCheck pos expr1 expr2 andTypes
typecheckExpr (EOr pos expr1 expr2) = binaryCheck pos expr1 expr2 orTypes


unaryCheck :: BNFC'Position -> Expr -> [Type] -> TCM Type
unaryCheck pos expr allowedTypes = do 
    exprType <- typecheckExpr expr
    unless (or $ map (compareTypes exprType) allowedTypes) (throwError $ TE pos (TEUnaryOp exprType))
    return exprType
        

comparePairs :: (Type, Type) -> (Type, Type) -> Bool
comparePairs (t1l, t1r) (t2l, t2r) = compareTypes t1l t2l && compareTypes t1r t2r

binaryCheck :: BNFC'Position -> Expr -> Expr -> [(Type, Type)] -> TCM Type
binaryCheck pos expr1 expr2 allowedTypes = do
    exprType1 <- typecheckExpr expr1
    exprType2 <- typecheckExpr expr2
    if or $ map (comparePairs (exprType1, exprType2)) allowedTypes then
        if isArray exprType2 then return exprType2 else return exprType1
    else
        throwError $ TE pos (TEBinaryOp exprType1 exprType2)

typecheckMut :: Mut -> TCM Type
typecheckMut (MutL pos ident) = do
    checkReadOnly pos ident
    getType pos ident
typecheckMut (MutArr pos ident indexExpr) = typecheckExpr (EGet pos ident indexExpr)

deriving instance Typeable (Type' a)
deriving instance Data a => Data (Type' a)

compareTypes :: Type -> Type -> Bool
compareTypes (Arr _ t1) (Arr _ t2) = compareTypes t1 t2
compareTypes (Fun _ argList1 returnType1) (Fun _ argList2 returnType2) = 
    (length argList1 == length argList2) && (and $ zipWith compareTypes (returnType1 : argList1) (returnType2 : argList2))
compareTypes t1 t2 = toConstr t1 == toConstr t2

isArray :: Type -> Bool
isArray t = or $ map (compareTypes t) arrays

int = Int Nothing
bool = Bool Nothing
str = Str Nothing
voidT = Void Nothing

simpleTypes = [int, bool, str, voidT]
arrays = map (\t -> Arr Nothing t) simpleTypes

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

negTypes = [int]
notTypes = [bool]
mulTypes = [(int, int)] ++ (cartProd [int] arrays) ++ (cartProd arrays [int])
addTypes = [(int, int)]
relTypes = [(int, int)]
andTypes = [(bool, bool)]
orTypes = andTypes