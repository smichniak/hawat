{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Typechecker where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Data (Typeable, Data, toConstr)

import AbsHawat
import TypecheckerEnv
import TypecheckerError

maybeError :: Either TypeError TypecheckEnv -> Maybe TypeError
maybeError (Left err) = Just err
maybeError (Right _) = Nothing

typecheckProgram :: Program -> Maybe TypeError -- Nothing if there were no errors
typecheckProgram (ProgramL _ topDefList) = maybeError $ (runReader $ runExceptT $ typecheckTopDefs topDefList) initTCEnv

typecheckTopDefs :: [TopDef] -> TCM TypecheckEnv
typecheckTopDefs [] = do
    declaredMain <- getType BNFC'NoPosition (Ident "main")
    unless (compareTypes declaredMain mainFunction) (throwError $ makeError declaredMain (TEMainSig declaredMain mainFunction))
    ask
typecheckTopDefs (x:xs) = do
    newEnv <- typecheckTopDef x
    local (const newEnv) (typecheckTopDefs xs)

typecheckTopDef :: TopDef -> TCM TypecheckEnv
typecheckTopDef (Global _ decl) = typecheckDecl decl
typecheckTopDef (FnDef pos ident argList returnType block) = typecheckFunction pos ident argList returnType block

typecheckDecl :: Decl -> TCM TypecheckEnv
typecheckDecl (DeclL _ _ []) = ask

typecheckDecl (DeclL declPos typ ((NoInit _ ident) : xs)) = do
    newEnv <- declare ident typ
    local (const newEnv) (typecheckDecl $ DeclL declPos typ xs)

typecheckDecl (DeclL declPos typ ((Init initPos ident expr) : xs)) = do
    newEnv <- declare ident typ
    _ <- local (const newEnv) (typecheckStmt (Ass initPos (EVar initPos ident) expr)) -- Can be discarded, we only care if errors are thrown
    local (const newEnv) (typecheckDecl $ DeclL declPos typ xs)

getArgType :: Arg -> Type
getArgType (ArgL _ typ _) = typ

functionType :: BNFC'Position -> [Arg] -> Type -> Type
functionType pos argList = Fun pos (map getArgType argList)

typecheckFunction :: BNFC'Position -> Ident -> [Arg] -> Type -> Block -> TCM TypecheckEnv
typecheckFunction pos ident argList returnType block = do
    newEnv <- declare ident (functionType pos argList returnType)
    argEnv <- local (addLevel . const newEnv) (declareArgs argList)
    _ <- local (setReturnType returnType . const argEnv) (typecheckBlock block)
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
    _ <- local (addLevel . const env) (typecheckBlock block)
    ask
typecheckStmt (SDecl _ decl) = typecheckDecl decl

typecheckStmt (Ass _ (EVar varPos ident) expr) = do
    checkReadOnly varPos ident
    exprType <- typecheckExpr expr
    lType <- getType varPos ident
    unless (compareTypes lType exprType) (throwError $ makeError expr (TEAssignment lType exprType))
    ask
typecheckStmt (Ass _ (EGet arrPos arrExpr indexExpr) expr) = do
    exprType <- typecheckExpr expr
    arrType <- typecheckExpr (EGet arrPos arrExpr indexExpr)
    unless (compareTypes arrType exprType) (throwError $ makeError expr (TEAssignment arrType exprType))
    ask
typecheckStmt (Ass pos _ _) = throwError $ TE pos TEAssignExp

typecheckStmt (Incr pos expr) = do
    exprType <- typecheckExpr expr
    case exprType of
        Int _ -> typecheckStmt (Ass pos expr (ELitInt pos 0)) -- 0 only a placeholder, ignored later
        t -> throwError $ TE pos (TEUnaryOp t)
typecheckStmt (Decr pos expr) = typecheckStmt (Incr pos expr)

typecheckStmt (Ret _ returnExpr) = do
    expectedType <- asks tceReturnType
    retType <- typecheckExpr returnExpr
    unless (compareTypes expectedType retType) (throwError $ makeError returnExpr (TEReturn retType expectedType))
    ask
typecheckStmt (VRet pos) = typecheckStmt (Ret pos (EVoid pos))
typecheckStmt (SCond _ cond) = do
    env <- ask
    _ <- local (addLevel . const env) (typecheckSCond cond)
    ask
typecheckStmt (While pos condExpr block) = do
    env <- ask
    _ <- local (goInsideLoop . addLevel . const env) (typecheckSCond (If pos condExpr block))
    ask
typecheckStmt (For pos ident startExpr finalExpr block) = do
    env <- ask
    startType <- typecheckExpr startExpr
    unless (compareTypes startType int)  (throwError $ makeError startExpr (TEForIter startType))
    endType <- typecheckExpr finalExpr
    unless (compareTypes endType int)  (throwError $ makeError finalExpr (TEForIter endType))
    loopEnv <- local (addLevel . const env) (declare ident (Int pos))
    _ <- local (addReadOnly ident . goInsideLoop . addLevel . const loopEnv) (typecheckSCond (If pos (ELitTrue pos) block))
    ask
typecheckStmt (LoopJmp _ loopS) = checkInLoop loopS >> ask
typecheckStmt (SExp _ expr) = typecheckExpr expr >> ask

typecheckSCond :: Cond -> TCM TypecheckEnv
typecheckSCond (If _ condExpr block) = do
    condType <- typecheckExpr condExpr
    unless (compareTypes condType bool) (throwError $ makeError condExpr (TECondtion condType))
    typecheckBlock block
typecheckSCond (IfElse pos condExpr block1 block2) = do
    _ <- typecheckSCond (If pos condExpr block1)
    typecheckBlock block2
typecheckSCond (ElseIf pos condExpr block alternativeCond) = do
    _ <- typecheckSCond (If pos condExpr block)
    typecheckSCond alternativeCond

typecheckExpr :: Expr -> TCM Type
typecheckExpr (EVar pos ident) = getType pos ident
typecheckExpr (ELitInt pos _) = do return $ Int pos
typecheckExpr (ELitTrue pos) = do return $ Bool pos
typecheckExpr (ELitFalse pos) = do return $ Bool pos
typecheckExpr (EVoid pos) = do return $ Void pos
typecheckExpr (EApp pos funExpr exprList) = do
    appliedTypes <- mapM typecheckExpr exprList
    funType <- typecheckExpr funExpr
    case funType of
        Fun _ argumentList returnType -> do
            let correctArguments = and $ (length appliedTypes == length argumentList) : zipWith compareTypes appliedTypes argumentList
            unless correctArguments (throwError $ TE pos (TEApplication appliedTypes argumentList))
            return returnType
        _ -> throwError $ makeError funExpr (TENotFunction funType)

typecheckExpr (EGet pos arrExpr indexExpr) = do
    containerType <- typecheckExpr arrExpr
    indexType <- typecheckExpr indexExpr
    case containerType of
        Arr _ arrType -> case indexType of
            Int _ -> return arrType
            typ -> throwError $ TE pos (TEArrIndex typ)
        _ -> throwError $ makeError arrExpr (TENotArray containerType)

typecheckExpr (EString pos _) = do return $ Str pos
typecheckExpr (ELambda pos argList returnType block) = do
    _ <- typecheckFunction pos (Ident "") argList returnType block
    return $ functionType pos argList returnType

-- TODO Typing empty array, add type 'Any' that compares with every other type
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
typecheckExpr (EMul pos expr1 (Times _) expr2) = do
    binaryCheck pos expr1 expr2 timesTypes
    exprType1 <- typecheckExpr expr1
    if isArray exprType1 then return exprType1 else typecheckExpr expr2
typecheckExpr (EMul pos expr1 _ expr2) = binaryCheck pos expr1 expr2 mulTypes >> return int
typecheckExpr (EAdd pos expr1 _ expr2) = binaryCheck pos expr1 expr2 addTypes >> return int
typecheckExpr (ERel pos expr1 _ expr2) = binaryCheck pos expr1 expr2 relTypes >> return bool
typecheckExpr (EAnd pos expr1 expr2) = binaryCheck pos expr1 expr2 andTypes >> return bool
typecheckExpr (EOr pos expr1 expr2) = binaryCheck pos expr1 expr2 orTypes >> return bool


unaryCheck :: BNFC'Position -> Expr -> [Type] -> TCM Type
unaryCheck pos expr allowedTypes = do
    exprType <- typecheckExpr expr
    unless (any (compareTypes exprType) allowedTypes) (throwError $ TE pos (TEUnaryOp exprType))
    return exprType


comparePairs :: (Type, Type) -> (Type, Type) -> Bool
comparePairs (t1l, t1r) (t2l, t2r) = compareTypes t1l t2l && compareTypes t1r t2r

binaryCheck :: BNFC'Position -> Expr -> Expr -> [(Type, Type)] -> TCM ()
binaryCheck pos expr1 expr2 allowedTypes = do
    exprType1 <- typecheckExpr expr1
    exprType2 <- typecheckExpr expr2
    unless (any (comparePairs (exprType1, exprType2)) allowedTypes) (throwError $ TE pos (TEBinaryOp exprType1 exprType2))

deriving instance Typeable (Type' a)
deriving instance Data a => Data (Type' a)

compareTypes :: Type -> Type -> Bool
compareTypes (Arr _ t1) (Arr _ t2) = compareTypes t1 t2
compareTypes (Fun _ argList1 returnType1) (Fun _ argList2 returnType2) =
    (length argList1 == length argList2) && and (zipWith compareTypes (returnType1 : argList1) (returnType2 : argList2))
compareTypes t1 t2 = toConstr t1 == toConstr t2

isArray :: Type -> Bool
isArray (Arr _ _) = True
isArray _ = False

int = Int BNFC'NoPosition
bool = Bool BNFC'NoPosition
str = Str BNFC'NoPosition
voidT = Void BNFC'NoPosition
mainFunction = Fun BNFC'NoPosition [] int

simpleTypes = [int, bool, str, voidT]
arrays = map (\t -> Arr (hasPosition t) t) simpleTypes

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

negTypes = [int]
notTypes = [bool]
timesTypes = [(int, int)] ++ (cartProd [int] arrays) ++ (cartProd arrays [int])
mulTypes = [(int, int)]
addTypes = [(int, int)]
relTypes = [(int, int)]
andTypes = [(bool, bool)]
orTypes = andTypes
